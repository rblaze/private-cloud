{-# Language OverloadedStrings, LambdaCase, RecordWildCards #-}
module Main where

import Control.Concurrent
import Control.Exception.Safe
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteArray (ScrubbedBytes)
import Data.Time.Clock
import Data.Word
import System.Console.Haskeline
import System.CredentialStore
import System.Directory
import System.Exit
import System.FilePath
import System.FilePath.Glob
import System.Log.Logger
import System.Random
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import PrivateCloud.Aws.Provider
import PrivateCloud.Cloud.Action
import PrivateCloud.Cloud.Monad hiding (cloudId)
import PrivateCloud.Provider.Class

import Options

mainLoggerName :: String
mainLoggerName = "PrivateCloud"

main :: IO ()
main = do
    options <- getOptions
    print options

    run options

encodeUtf :: String -> BS.ByteString
encodeUtf = T.encodeUtf8 . T.pack

whileNothing :: Monad m => m (Maybe b) -> m b
whileNothing prompt = do
    resp <- prompt
    case resp of
        Just v -> return v
        Nothing -> whileNothing prompt

run :: Options -> IO ()
run Create{..} = do
    dbExists <- doesPathExist (root </> dbName)
    when dbExists $ do
        putStrLn $ "Local database already exists at " ++ root
            ++ ", can't create new cloud instance"
        exitFailure

    instanceId <- if null cloudId
                then do
                    randomId <- getStdRandom random
                    return $ show (randomId :: Word32)
                else
                    return cloudId

    putStrLn $ "Creating cloud instance " ++ instanceId

    (rootKeyId, rootSecretKey) <-
        runInputT (defaultSettings { autoAddHistory = False }) $ do
            keyid <- if null adminKeyId
                then whileNothing $ getInputLine "Admin AccessKeyId: "
                else return adminKeyId
            secret <- if null adminSecretKey
                then whileNothing $ getPassword (Just '*') "Admin SecretKey: "
                else return adminSecretKey
            return (encodeUtf keyid, encodeUtf secret)

    createDirectoryIfMissing True root
    credentials <- setupAwsPrivateCloud root (T.pack instanceId) rootKeyId rootSecretKey
    withCredentialStore $ \store ->
        let credName = "privatecloud-" ++ instanceId
         in putCredential store credName (credentials :: ScrubbedBytes)

run Connect{..} = undefined

run Run{..} = do
    let fullSyncDelay = fromIntegral (fullSyncInterval * 60)
    let cleanupDelay = fromIntegral (cleanupInterval * 60)

    updateGlobalLogger mainLoggerName (setLevel loglevel)

    noticeM mainLoggerName $ "#START #root " ++ root

    patterns <- forM (dbName : exclPatterns) $ \pat -> do
        case simplify <$> tryCompileWith compPosix pat of
            Left errmsg -> do
                errorM mainLoggerName $ "#EXCLUSION_ERROR #pattern " ++ show pat
                    ++ " #msg " ++ errmsg
                exitFailure
            Right pattern -> do
                infoM mainLoggerName $ "#EXCLUSION #pattern " ++ show pat
                return pattern

    let getCred cloudid =
            let credName = "privatecloud-" ++ T.unpack cloudid
             in withCredentialStore $ \store ->
                getCredential store credName :: IO (Maybe ScrubbedBytes)

    runAwsPrivateCloud root patterns getCred $ do
        liftIO $ noticeM mainLoggerName "#RUN"

        syncAllChanges
        startTime <- liftIO getCurrentTime

        let loop lastFullSyncTime lastCleanupTime = handleAny
                (\e -> liftIO $ errorM mainLoggerName $ "#EXCEPTION #msg " ++ show e) $
              do
                liftIO $ threadDelay (60000000 * fromIntegral syncInterval)
                currentTime <- liftIO getCurrentTime

                let sinceLastFullSync = diffUTCTime currentTime lastFullSyncTime
                lfst <- if sinceLastFullSync > fullSyncDelay
                    then do
                        syncAllChanges
                        return currentTime
                    else do
                        liftIO $ noticeM mainLoggerName $ "#TIMER #tillNextFullSync " ++ show (fullSyncDelay - sinceLastFullSync)
                        syncRecentChanges
                        return lastFullSyncTime

                let sinceLastCleanup = diffUTCTime currentTime lastCleanupTime
                lct <- if sinceLastCleanup > cleanupDelay
                    then do
                        ctx <- context
                        runCloud ctx cleanupCloud
                        return currentTime
                    else do
                        liftIO $ noticeM mainLoggerName $ "#TIMER #tillNextCleanup " ++ show (cleanupDelay - sinceLastCleanup)
                        return lastCleanupTime
                loop lfst lct

        loop startTime startTime
