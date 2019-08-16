{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module PrivateCloud.Aws.Cleanup
    ( deleteOldDbRecords
    , deleteOldVersions
    ) where

import PrivateCloud.Aws.Logging
import PrivateCloud.Aws.Monad
import PrivateCloud.Aws.SimpleDb
import PrivateCloud.Aws.Util
import PrivateCloud.Provider.Types

import Aws.Aws
import Aws.S3
import Aws.SimpleDb
import Conduit
import Control.Monad (void)
import Data.Maybe (mapMaybe)
import Data.Time.Clock (NominalDiffTime, diffUTCTime)
import Data.Time.Clock.POSIX (getPOSIXTime, getCurrentTime)
import qualified Data.HashSet as HS

-- Purge DB records marked as deleted more than 24 hours ago.
maxUnusedTime :: NominalDiffTime
maxUnusedTime = 86400

deleteOldDbRecords :: AwsMonad ()
deleteOldDbRecords = do
    sdbLogInfo "DB_CLEANUP_START"
    domain <- acDomain <$> awsContext
    time <- liftIO getPOSIXTime
    let timestr = printTime $ time - maxUnusedTime
    let querystr = "select recordmtime from `" <> domain <>
                "` where hash = '' intersection recordmtime < '" <> timestr <> "'"
    let query = (select querystr) { sConsistentRead = True }
    runConduit $ awsIteratedList' awsReq query
        .| mapM_C (deleteDbRecord domain)
    sdbLogInfo "DB_CLEANUP_END"
  where
    deleteDbRecord domain Item{..} = case itemData of
        [ ForAttribute "recordmtime" mtime ] -> do
            sdbLogInfo $ "DB_DELETE_RECORD #file " ++ show itemName
            void $ awsReq (deleteAttributes itemName [] domain)
                { daExpected = [ expectedValue "recordmtime" mtime ]
                }
        _ -> sdbLogCritical $ "invalid attribute list " ++ show itemData

-- Delete all unreferenced objects older than 24 hours: time between
-- upload and database write should not be that long.
deleteOldVersions :: AwsMonad ()
deleteOldVersions = do
    sdbLogInfo "S3_CLEANUP_START"
    filelist <- getAllServerFiles
    currentTime <- liftIO getCurrentTime
    let isCloudFile (_, CloudDeleteMarker) = Nothing
        isCloudFile (_, CloudFile i) = Just (cfStorageId i)
    let knownObjects = HS.fromList $ mapMaybe isCloudFile filelist

    bucket <- acBucket <$> awsContext
    runConduit $ awsIteratedList' awsReq (getBucket bucket)
        .| filterMC (checkObject currentTime knownObjects)
        .| mapM_C (deleteObj bucket)
    sdbLogInfo "S3_CLEANUP_END"
  where
    checkObject currentTime knownObjects ObjectInfo{..}
        | HS.member (StorageId objectKey) knownObjects = do
            sdbLogInfo $ "S3_CLEANUP_KNOWN #objid " ++ show objectKey
            pure False
        | otherwise = do
            let age = diffUTCTime currentTime objectLastModified
            let tooOld = age > maxUnusedTime
            if tooOld
                then sdbLogInfo $ "S3_CLEANUP_OLD #objid " ++ show objectKey ++ " #age " ++ show age
                else sdbLogInfo $ "S3_CLEANUP_RECENT #objid " ++ show objectKey ++ " #age " ++ show age
            pure tooOld

    deleteObj bucket ObjectInfo{..} = do
        sdbLogInfo $ "S3_DELETE #objid " ++ show objectKey
        void $ awsReq $ DeleteObject objectKey bucket
