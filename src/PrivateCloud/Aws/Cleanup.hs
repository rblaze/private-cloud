{-# Language OverloadedStrings, RecordWildCards, MultiWayIf #-}
module PrivateCloud.Aws.Cleanup where

import Conduit
import Control.Lens
import Control.Monad
import Control.Monad.Trans.Resource
import Data.Maybe
import Data.Monoid
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Network.AWS (send, paginate)
import Network.AWS.S3
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS
import qualified Data.HashSet as HS

import PrivateCloud.Aws.Logging
import PrivateCloud.Aws.Monad
import PrivateCloud.Aws.SimpleDb
import PrivateCloud.Provider.Types

import Aws.Aws
import Aws.Core
import Aws.SimpleDb

data S3Version = S3Version
    { s3Key :: ObjectKey
    , s3LastModified :: UTCTime
    }

-- 24 hours
maxUnusedTime :: NominalDiffTime
maxUnusedTime = 86400

deleteOldVersions :: AwsMonad ()
deleteOldVersions = do
    awsLogInfo "#S3CLEANUP_START"
    filelist <- getAllServerFiles
    currentTime <- liftIO getCurrentTime
    let isCloudFile (_, CloudDeleteMarker) = Nothing
        isCloudFile (_, CloudFile i) = Just (cfStorageId i)
    let knownObjects = HS.fromList $ mapMaybe isCloudFile filelist

    -- delete all unreferenced objects older than 24 hours:
    --      time between upload and database write should not be that long
    let checkObject info
            | HS.member storageId knownObjects = do
                awsLogInfo $ "#S3CLEANUP_KNOWN #objid " ++ show storageId
                return False
            | otherwise = do
                let age = diffUTCTime currentTime (s3LastModified info)
                let tooOld = age > maxUnusedTime
                if tooOld
                    then awsLogInfo $ "#S3CLEANUP_OLD #objid " ++ show storageId ++ " #age " ++ show age
                    else awsLogInfo $ "#S3CLEANUP_RECENT #objid " ++ show storageId ++ " #age " ++ show age
                return tooOld
            where
            ObjectKey key = s3Key info
            storageId = StorageId key

    bucketName <- awsAsks acBucket
    runConduit $ paginate (listObjects bucketName)
        .| unpack
        .| filterMC checkObject
        .| mapM_C deleteObj
    awsLogInfo "#S3CLEANUP_END"
    where
    unpack = awaitForever $ \resp ->
                forM_ (resp ^. lorsContents) $ \m ->
                        yield $ S3Version (m ^. oKey) (m ^. oLastModified)
    deleteObj info = do
        let key = s3Key info
        awsLogNotice $ "#S3DELETE #objid " ++ show key
        bucketName <- awsAsks acBucket
        void $ send $ deleteObject bucketName key

deleteOldDbRecords :: AwsMonad ()
deleteOldDbRecords = do
    awsLogInfo "#DBCLEANUP_START"
    time <- liftIO getPOSIXTime
    domain <- awsAsks acDomain
    let timestr = printTime $ time - maxUnusedTime
    let querystr = "select recordmtime from `" <> domain <>
                "` where hash = '' intersection recordmtime < '" <> timestr <> "'"
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
