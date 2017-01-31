{-# Language RecordWildCards, MultiWayIf #-}
module PrivateCloud.Aws.Cleanup where

import Aws.Aws
import Aws.Core
import Aws.S3
import Conduit
import Control.Monad
import Control.Monad.Trans.State.Strict
import Data.Maybe
import Data.Time.Clock
import System.Log.Logger
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

import PrivateCloud.Aws
import PrivateCloud.Aws.S3
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
    infoM s3LoggerName "#S3CLEANUP_START"
    filelist <- getServerFiles config
    let isCloudFile (_, CloudDeleteMarker) = Nothing
        isCloudFile (f, CloudFile i) = Just (T.pack f, cfVersion i)
    let knownVersions = HM.fromList $ mapMaybe isCloudFile filelist
    let command = getBucketObjectVersions ciBucket

    -- delete all versions older than current one
    -- delete all versions older than 24 hours in unknown files or before current one:
    --      time between upload and database write should not be that long
    let checkCurrentFile st info
            | seenStoredVersion st = return True
            | version == storedVersion st = do
                logInfo $ "#S3CLEANUP_FOUNDCURRENT #file " ++ show key ++ " #version " ++ show version
                put $ st { seenStoredVersion = True }
                return False
            | otherwise = do
                time <- liftIO getCurrentTime
                let age = diffUTCTime time (oviLastModified info)
                let tooOld = age > maxUnusedTime
                unless tooOld $ logInfo $ "#S3CLEANUP_RECENT #file " ++ show key ++ " #version " ++ show version ++ " #age " ++ show age
                return tooOld
            where
            key = oviKey info
            version = VersionId $ oviVersionId info

    let checkNextFile info
            | isNothing dbinfo = do
                time <- liftIO getCurrentTime
                let age = diffUTCTime time (oviLastModified info)
                let tooOld = age > maxUnusedTime
                logInfo $ "#S3CLEANUP_UNKNOWN #file " ++ show key ++ " #version " ++ show version ++ " #age " ++ show age
                return tooOld
            | otherwise = do
                let isCurrent = version == dbVersion
                logInfo $ "#S3CLEANUP_KNOWN #file " ++ show key ++ " #version " ++ show version ++ " #isCurrent " ++ show isCurrent
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
            dbinfo = HM.lookup key knownVersions
            Just dbVersion = dbinfo

    let checkVersion info = do
            st <- get
            if oviKey info == currentKey st
                then checkCurrentFile st info
                else checkNextFile info

    let baseState = CleanupState
            { currentKey = T.empty
            , storedVersion = noVersion
            , seenStoredVersion = False
            }
    runResourceT $ flip evalStateT baseState $ runConduit $ awsIteratedList'
            (\r -> readResponseIO =<< lift (aws ciConfig defServiceConfig ciManager r))
            command
        .| filterMC checkVersion
        .| mapM_C deleteVersion
    logInfo "#S3CLEANUP_END"
    where
    deleteVersion info = liftIO $ do
        let key = oviKey info
        let version = oviVersionId info
        let delCommand = deleteObjectVersion ciBucket key version
        noticeM s3LoggerName $ "#S3DELETEVERSION #file " ++ show key
            ++ " #version " ++ show version
        void $ memoryAws ciConfig defServiceConfig ciManager delCommand
    logInfo :: MonadIO m => String -> m ()
    logInfo = liftIO . infoM s3LoggerName
