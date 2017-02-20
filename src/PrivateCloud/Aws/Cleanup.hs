{-# Language OverloadedStrings, RecordWildCards, MultiWayIf #-}
module PrivateCloud.Aws.Cleanup where

import Conduit
import Control.Lens
import Control.Monad
import Control.Monad.Trans.Resource
import Control.Monad.Trans.State.Strict
import Data.Maybe
import Data.Monoid
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Network.AWS (send, paginate, liftAWS)
import Network.AWS.S3
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

import PrivateCloud.Aws.Logging
import PrivateCloud.Aws.Monad
import PrivateCloud.Aws.SimpleDb
import PrivateCloud.Provider.FileInfo

import Aws.Aws
import Aws.Core
import Aws.SimpleDb

data CleanupState = CleanupState
    { currentKey :: ObjectKey
    , storedVersion :: ObjectVersionId
    , seenStoredVersion :: Bool
    }

data S3Version = S3Version
    { s3Key :: ObjectKey
    , s3Version :: ObjectVersionId
    , s3LastModified :: UTCTime
    }

noVersion :: VersionId
noVersion = VersionId T.empty

-- 24 hours
maxUnusedTime :: NominalDiffTime
maxUnusedTime = 86400

deleteOldVersions :: AwsMonad ()
deleteOldVersions = do
    awsLogInfo "#S3CLEANUP_START"
    filelist <- getAllServerFiles
    let isCloudFile (_, CloudDeleteMarker) = Nothing
        isCloudFile (f, CloudFile i) = Just (f, cfVersion i)
    let knownVersions = HM.fromList $ mapMaybe isCloudFile filelist

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
                let age = diffUTCTime time (s3LastModified info)
                let tooOld = age > maxUnusedTime
                unless tooOld $ awsLogInfo $ "#S3CLEANUP_RECENT #file " ++ show key ++ " #version " ++ show version ++ " #age " ++ show age
                return tooOld
            where
            key = s3Key info
            version = s3Version info

    let checkNextFile info
            | isNothing dbinfo = do
                time <- liftIO getCurrentTime
                let age = diffUTCTime time (s3LastModified info)
                let tooOld = age > maxUnusedTime
                awsLogInfo $ "#S3CLEANUP_UNKNOWN #file " ++ show key ++ " #version " ++ show version ++ " #age " ++ show age
                return tooOld
            | otherwise = do
                let isCurrent = version == latestVersion
                awsLogInfo $ "#S3CLEANUP_KNOWN #file " ++ show key ++ " #version " ++ show version ++ " #isCurrent " ++ show isCurrent
                put CleanupState
                    { currentKey = key
                    , storedVersion = latestVersion
                    , seenStoredVersion = isCurrent
                    }
                if isCurrent
                    then return False
                    else do
                        time <- liftIO getCurrentTime
                        let age = diffUTCTime time (s3LastModified info)
                        return $ age > maxUnusedTime
            where
            key = s3Key info
            version = s3Version info
            latestVersion = let VersionId v = dbVersion in ObjectVersionId v
            dbKey = let ObjectKey v = key in EntryName v
            dbinfo = HM.lookup dbKey knownVersions
            Just dbVersion = dbinfo

    let checkVersion info = do
            st <- get
            if s3Key info == currentKey st
                then checkCurrentFile st info
                else checkNextFile info

    bucketName <- awsAsks acBucket
    flip evalStateT baseState $
        runConduit $ paginate (listObjectVersions bucketName)
        .| unpack
        .| filterMC checkVersion
        .| mapM_C deleteVersion
    awsLogInfo "#S3CLEANUP_END"
    where
    baseState = CleanupState
                { currentKey = ObjectKey T.empty
                , storedVersion = ObjectVersionId T.empty
                , seenStoredVersion = False
                }
    unpack = let loop = do
                    mx <- await
                    case mx of
                        Nothing -> return ()
                        Just resp -> do
                            mapM_ yield $
                                flip mapMaybe (resp ^. lovrsDeleteMarkers) $
                                    \m -> S3Version <$> (m ^. dmeKey) <*> (m ^. dmeVersionId) <*> (m ^. dmeLastModified)
                            mapM_ yield $
                                flip mapMaybe (resp ^. lovrsVersions) $
                                    \m -> S3Version <$> (m ^. ovKey) <*> (m ^. ovVersionId) <*> (m ^. ovLastModified)
                            loop
              in loop
    deleteVersion info = do
        let key = s3Key info
        let version = s3Version info
        awsLogNotice $ "#S3DELETEVERSION #file " ++ show key
            ++ " #version " ++ show version
        bucketName <- lift $ awsAsks acBucket
        void $ liftAWS $ send $ deleteObject bucketName key & doVersionId ?~ version

deleteOldDbRecords :: AwsMonad ()
deleteOldDbRecords = do
    awsLogInfo "#DBCLEANUP_START"
    time <- liftIO getPOSIXTime
    domain <- awsAsks acDomain
    let timestr = printTime $ time - maxUnusedTime
    let querystr = "select recordmtime from " <> domain <>
                " where hash = '' intersection recordmtime < '" <> timestr <> "'"
    let query = (select querystr) { sConsistentRead = True }
    conf <- awsAsks acLegacyConf
    manager <- liftIO $ newManager tlsManagerSettings
    liftResourceT $ runConduit $
        awsIteratedList conf defServiceConfig manager query
        .| mapM_C (deleteDbRecord conf manager domain)
    awsLogInfo "#DBCLEANUP_END"
    where
    deleteDbRecord conf manager domain Item{..} = case itemData of
        [ ForAttribute "recordmtime" mtime ] -> do
            awsLogNotice $ "#S3DELETERECORD #file " ++ show itemName
            void $ pureAws conf defServiceConfig manager $
                (deleteAttributes itemName [] domain)
                { daExpected = [ expectedValue "recordmtime" mtime ]
                }
        _ -> awsLogCritical $ "invalid attribute list " ++ show itemData
