{-# Language OverloadedStrings, LambdaCase, RecordWildCards #-}
module Main where

import Control.Concurrent
import Control.Exception.Safe
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteArray (ScrubbedBytes)
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Word
import System.Console.Haskeline hiding (bracket)
import System.CredentialStore
import System.Directory
import System.Exit
import System.FileLock
import System.FilePath
import System.FilePath.Glob
import System.Log.Logger
import System.Random
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import PrivateCloud.Aws.Provider
import PrivateCloud.Cloud.Action
import PrivateCloud.Cloud.EventLog
import PrivateCloud.Cloud.Monad as Cloud
import PrivateCloud.Provider.Class

import Options

appLoggerName :: String
appLoggerName = "PrivateCloud.App"

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
run Create{cloudId = cloudid, ..} = do
    dbExists <- doesPathExist (root </> dbName)
    when dbExists $ do
        putStrLn $ "Local database already exists at " ++ root
            ++ ", can't create new cloud instance"
        exitFailure

    instanceId <- if null cloudid
                then do
                    randomId <- getStdRandom random
                    return $ show (randomId :: Word32)
                else
                    return cloudid
    userId <- do
        randomId <- getStdRandom random
        return $ show (randomId :: Word32)

    putStrLn $ "Creating cloud instance " ++ instanceId ++ " with user " ++ userId

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
    (uniqueId, credentials) <- setupAwsPrivateCloud root (T.pack instanceId) (T.pack userId) rootKeyId rootSecretKey
    withCredentialStore $ \store ->
        let credName = "privatecloud-" ++ T.unpack uniqueId
         in putCredential store credName (credentials :: ScrubbedBytes)

run Connect{cloudId = cloudid, ..} = do
    dbExists <- doesPathExist (root </> dbName)
    when dbExists $ do
        putStrLn $ "Local database already exists at " ++ root
            ++ ", can't create new cloud instance"
        exitFailure

    (instanceId, rootKeyId, rootSecretKey) <-
        runInputT (defaultSettings { autoAddHistory = False }) $ do
            instanceid <- if null cloudid
                then whileNothing $ getInputLine "Cloud instance: "
                else return cloudid
            keyid <- if null adminKeyId
                then whileNothing $ getInputLine "Admin AccessKeyId: "
                else return adminKeyId
            secret <- if null adminSecretKey
                then whileNothing $ getPassword (Just '*') "Admin SecretKey: "
                else return adminSecretKey
            return (instanceid, encodeUtf keyid, encodeUtf secret)

    userId <- do
        randomId <- getStdRandom random
        return $ show (randomId :: Word32)

    putStrLn $ "Connecting to cloud instance " ++ instanceId ++ " as user " ++ userId

    createDirectoryIfMissing True root
    (uniqueId, credentials) <- connectAwsPrivateCloud root (T.pack instanceId) (T.pack userId) rootKeyId rootSecretKey
    withCredentialStore $ \store ->
        let credName = "privatecloud-" ++ T.unpack uniqueId
         in putCredential store credName (credentials :: ScrubbedBytes)

run Run{..} = do
    let fullSyncDelay = fromIntegral (fullSyncInterval * 60)
    let cleanupDelay = fromIntegral (cleanupInterval * 60)
    let lockName = ".privatecloud.lock"

    -- GUI can register custom handler for eventLoggerName and display notifications
    -- Here we just write everything to stderr
    updateGlobalLogger eventLoggerName (setLevel NOTICE)
    -- Also print debugging loggers at level requested
    updateGlobalLogger "PrivateCloud" (setLevel loglevel)

    let conflictPattern = "*" ++  conflictSuffix
    patterns <- forM (conflictPattern : lockName : dbName : exclPatterns) $ \pat -> do
        case simplify <$> tryCompileWith compPosix pat of
            Left errmsg -> do
                logEventError $ "INVALID_PATTERN #pattern " ++ show pat
                    ++ " #msg " ++ errmsg
                exitFailure
            Right pattern -> do
                infoM appLoggerName $ "#EXCLUSION #pattern " ++ show pat
                return pattern

    let getCred uniqueId =
            let credName = "privatecloud-" ++ T.unpack uniqueId
             in withCredentialStore $ \store ->
                getCredential store credName :: IO (Maybe ScrubbedBytes)

    let lockOrDie = tryLockFile (root </> lockName) Exclusive >>= \case
            Nothing -> do
                putStrLn $ "Service already running in " ++ root
                exitFailure
            Just lock -> return lock

    bracket lockOrDie unlockFile $ const $ runAwsPrivateCloud root patterns getCred $ do
        instanceId <- Cloud.cloudId
        logEventNotice $ "START #root " ++ root ++ " #instance " ++ T.unpack instanceId

        let loop lastFullSyncTime lastCleanupTime = do
                (lfst, lct) <- catchAny (step lastFullSyncTime lastCleanupTime) $
                    \e -> do
                        logEventError $ "#EXCEPTION #msg " ++ show e
                        -- Keep full sync time, but delay database cleanup.
                        -- This way after cloud outage all clients will not
                        -- try to do database cleanup at once.
                        currentTime <- liftIO getCurrentTime
                        return (lastFullSyncTime, currentTime)
                liftIO $ threadDelay (1000000 * fromIntegral syncInterval)
                loop lfst lct

            step lastFullSyncTime lastCleanupTime = do
                currentTime <- liftIO getCurrentTime

                let sinceLastFullSync = diffUTCTime currentTime lastFullSyncTime
                lfst <- if sinceLastFullSync > fullSyncDelay
                    then do
                        syncAllChanges
                        return currentTime
                    else do
                        liftIO $ noticeM appLoggerName $ "#TIMER #tillNextFullSync " ++ show (fullSyncDelay - sinceLastFullSync)
                        syncRecentChanges
                        return lastFullSyncTime

                let sinceLastCleanup = diffUTCTime currentTime lastCleanupTime
                lct <- if sinceLastCleanup > cleanupDelay
                    then do
                        ctx <- context
                        runCloud ctx cleanupCloud
                        return currentTime
                    else do
                        liftIO $ noticeM appLoggerName $ "#TIMER #tillNextCleanup " ++ show (cleanupDelay - sinceLastCleanup)
                        return lastCleanupTime

                return (lfst, lct)

        -- Force first sync to be full, but delay cleanup.
        startTime <- liftIO getCurrentTime
        loop (posixSecondsToUTCTime 0) startTime
