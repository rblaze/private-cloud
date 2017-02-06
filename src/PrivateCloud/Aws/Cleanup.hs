{-# Language OverloadedStrings, RecordWildCards, MultiWayIf #-}
module PrivateCloud.Aws.Cleanup where

import Aws.Aws
import Aws.Core
import Aws.S3
import Aws.SimpleDb
import Conduit
import Control.Monad
import Control.Monad.Trans.State.Strict
import Data.Maybe
import Data.Monoid
import Data.Time.Clock
import Data.Time.Clock.POSIX
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

import PrivateCloud.Aws
import PrivateCloud.Aws.SimpleDb
import PrivateCloud.FileInfo

data CleanupState = CleanupState
    { currentKey :: T.Text
    , storedVersion :: VersionId
    , seenStoredVersion :: Bool
    }

noVersion :: VersionId
noVersion = VersionId T.empty

-- 24 hours
maxUnusedTime :: NominalDiffTime
maxUnusedTime = 86400

deleteOldVersions :: CloudInfo -> IO ()
deleteOldVersions config@CloudInfo{..} = do
    awsLogInfo "#S3CLEANUP_START"
    filelist <- getAllServerFiles config
    let isCloudFile (_, CloudDeleteMarker) = Nothing
        isCloudFile (f, CloudFile i) = Just (f, cfVersion i)
    let knownVersions = HM.fromList $ mapMaybe isCloudFile filelist
    let command = getBucketObjectVersions ciBucket

    -- delete all versions older than current one
    -- delete all versions older than 24 hours in unknown files or before current one:
    --      time between upload and database write should not be that long
    let checkCurrentFile st info
            | seenStoredVersion st = return True
            | version == storedVersion st = do
                awsLogInfo $ "#S3CLEANUP_FOUNDCURRENT #file " ++ show key ++ " #version " ++ show version
                put $ st { seenStoredVersion = True }
                return False
            | otherwise = do
                time <- liftIO getCurrentTime
                let age = diffUTCTime time (oviLastModified info)
                let tooOld = age > maxUnusedTime
                unless tooOld $ awsLogInfo $ "#S3CLEANUP_RECENT #file " ++ show key ++ " #version " ++ show version ++ " #age " ++ show age
                return tooOld
            where
            key = oviKey info
            version = VersionId $ oviVersionId info

    let checkNextFile info
            | isNothing dbinfo = do
                time <- liftIO getCurrentTime
                let age = diffUTCTime time (oviLastModified info)
                let tooOld = age > maxUnusedTime
                awsLogInfo $ "#S3CLEANUP_UNKNOWN #file " ++ show key ++ " #version " ++ show version ++ " #age " ++ show age
                return tooOld
            | otherwise = do
                let isCurrent = version == dbVersion
                awsLogInfo $ "#S3CLEANUP_KNOWN #file " ++ show key ++ " #version " ++ show version ++ " #isCurrent " ++ show isCurrent
                put CleanupState
                    { currentKey = key
                    , storedVersion = dbVersion
                    , seenStoredVersion = isCurrent
                    }
                if isCurrent
                    then return False
                    else do
                        time <- liftIO getCurrentTime
                        let age = diffUTCTime time (oviLastModified info)
                        return $ age > maxUnusedTime
            where
            key = oviKey info
            version = VersionId $ oviVersionId info
            dbinfo = HM.lookup (EntryName key) knownVersions
            Just dbVersion = dbinfo

    let checkVersion info = do
            st <- get
            if oviKey info == currentKey st
                then checkCurrentFile st info
                else checkNextFile info

    runResourceT $ flip evalStateT baseState $ runConduit $ awsIteratedList'
            (lift . pureAws ciConfig defServiceConfig ciManager)
            command
        .| filterMC checkVersion
        .| mapM_C deleteVersion
    awsLogInfo "#S3CLEANUP_END"
    where
    baseState = CleanupState
                { currentKey = T.empty
                , storedVersion = noVersion
                , seenStoredVersion = False
                }
    deleteVersion info = do
        let key = oviKey info
        let version = oviVersionId info
        let delCommand = deleteObjectVersion ciBucket key version
        awsLogNotice $ "#S3DELETEVERSION #file " ++ show key
            ++ " #version " ++ show version
        void $ lift $ pureAws ciConfig defServiceConfig ciManager delCommand

deleteOldDbRecords :: CloudInfo -> IO ()
deleteOldDbRecords CloudInfo{..} = do
    awsLogInfo "#DBCLEANUP_START"
    time <- getPOSIXTime
    let timestr = printTime $ time - maxUnusedTime
    let querystr = "select recordmtime from " <> ciDomain <>
                " where hash = '' intersection recordmtime < '" <> timestr <> "'"
    let query = (select querystr) { sConsistentRead = True }
    runConduitRes $
        awsIteratedList ciConfig defServiceConfig ciManager query
        .| mapM_C deleteDbRecord
    awsLogInfo "#DBCLEANUP_END"
    where
    deleteDbRecord Item{..} = case itemData of
        [ ForAttribute "recordmtime" mtime ] -> do
            awsLogNotice $ "#S3DELETERECORD #file " ++ show itemName
            void $ pureAws ciConfig defServiceConfig ciManager $
                (deleteAttributes itemName [] ciDomain)
                { daExpected = [ expectedValue "recordmtime" mtime ]
                }
        _ -> awsLogCritical $ "invalid attribute list " ++ show itemData
