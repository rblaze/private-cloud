{-# Language OverloadedStrings, LambdaCase, RecordWildCards #-}
module Main where

import Control.Concurrent
import Control.Exception.Safe
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Data.ByteArray (ScrubbedBytes)
import Data.Time.Clock
import Data.Word
import Network.AWS
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

import PrivateCloud.Action
import PrivateCloud.AmazonWebServices
import PrivateCloud.Aws.Cleanup
import PrivateCloud.LocalDb
import PrivateCloud.Monad
import PrivateCloud.ServiceConfig
import PrivateCloud.CloudProvider

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

    account <- if null accountId
                then do
                    randomId <- getStdRandom random
                    return $ show (randomId :: Word32)
                else
                    return accountId

    let accountName = "privatecloud-" ++ account
    putStrLn $ "Creating account " ++ accountName

    (rootKeyId, rootSecretKey) <-
        runInputT (defaultSettings { autoAddHistory = False }) $ do
            keyid <- if null adminKeyId
                then whileNothing $ getInputLine "Admin AccessKeyId: "
                else return adminKeyId
            secret <- if null adminSecretKey
                then whileNothing $ getPassword (Just '*') "Admin SecretKey: "
                else return adminSecretKey
            return (AccessKey $ encodeUtf keyid, SecretKey $ encodeUtf secret)

    env <- newEnv $ FromKeys rootKeyId rootSecretKey
    credentials <- runResourceT $ runAwsCloud env $ createCloudInstance $ T.pack account

    withCredentialStore $ \store ->
        putCredential store accountName (credentials :: ScrubbedBytes)

    runPrivateCloudT root $ do
        initDatabase
        writeSetting "account" account

run Connect{..} = undefined

run Run{..} = do
    let fullSyncDelay = fromIntegral (fullSyncInterval * 60)
    let cleanupDelay = fromIntegral (cleanupInterval * 60)

    updateGlobalLogger mainLoggerName (setLevel loglevel)

    noticeM mainLoggerName $ "#START #root " ++ root

    exclusions <- forM (dbName : exclPatterns) $ \pat -> do
        case simplify <$> tryCompileWith compPosix pat of
            Left errmsg -> do
                errorM mainLoggerName $ "#EXCLUSION_ERROR #pattern " ++ show pat
                    ++ " #msg " ++ errmsg
                exitFailure
            Right pattern -> do
                infoM mainLoggerName $ "#EXCLUSION #pattern " ++ show pat
                return pattern

    withServiceConfig root "devtest" exclusions $ \config -> do
        noticeM mainLoggerName "#DBOPEN"

        runPrivateCloudT root $ syncAllChanges config
        startTime <- getCurrentTime

        void $ flip runStateT (startTime, startTime) $ forever $ handleAny
            (\e -> lift $ errorM mainLoggerName $ "#EXCEPTION #msg " ++ show e) $
            do
                lift $ threadDelay (60000000 * fromIntegral syncInterval)
                currentTime <- lift $ getCurrentTime
                sinceLastFullSync <- diffUTCTime currentTime <$> gets fst

                if sinceLastFullSync > fullSyncDelay
                    then do
                        lift $ runPrivateCloudT root $ syncAllChanges config
                        modify $ \(_, lastTime) -> (currentTime, lastTime)
                    else lift $ do
                        noticeM mainLoggerName $ "#TIMER #tillNextFullSync " ++ show (fullSyncDelay - sinceLastFullSync)
                        runPrivateCloudT root $ syncRecentChanges config

                sinceLastCleanup <- diffUTCTime currentTime <$> gets snd
                if sinceLastCleanup > cleanupDelay
                    then do
                        lift $ deleteOldVersions config
                        lift $ deleteOldDbRecords config
                        modify $ \(lastTime, _) -> (lastTime, currentTime)
                    else
                        lift $ noticeM mainLoggerName $ "#TIMER #tillNextCleanup " ++ show (cleanupDelay - sinceLastCleanup)
