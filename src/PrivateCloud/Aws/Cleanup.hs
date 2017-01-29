{-# Language RecordWildCards, MultiWayIf #-}
module PrivateCloud.Aws.Cleanup where

import Aws.Aws
import Aws.Core
import Aws.S3
import Conduit
import Control.Monad
import Control.Monad.Trans.State.Strict
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

deleteOldVersions :: CloudInfo -> IO ()
deleteOldVersions config@CloudInfo{..} = do
    infoM s3LoggerName "#S3CLEANUP_START"
    filelist <- getServerFiles config
    let knownVersions = HM.fromList $ map (\(f, i) -> (T.pack f, cfVersion i)) filelist
    let command = getBucketObjectVersions ciBucket
    let checkFile info = do
            let key = oviKey info
            let version = oviVersionId info
            st <- get
            if key == currentKey st
                then if | seenStoredVersion st -> liftIO $ deleteVersion key version
                        | VersionId version == storedVersion st -> do
                            logInfo $ "#S3CLEANUP_FOUNDCURRENT #file " ++ show key ++ " #version " ++ show version
                            put $ st { seenStoredVersion = True }
                        | otherwise -> return ()
                else case HM.lookup key knownVersions of
                        Just v -> do
                            let isCurrent = VersionId version == v
                            logInfo $ "#S3CLEANUP_KNOWN #file " ++ show key ++ " #version " ++ show version ++ " #isCurrent " ++ show isCurrent
                            put $ CleanupState
                                    { currentKey = key
                                    , storedVersion = v
                                    , seenStoredVersion = isCurrent
                                    }
                        Nothing ->  do
                            logInfo $ "#S3CLEANUP_UNKNOWN #file " ++ show key
                            -- unknown file, delete all versions
                            -- FIXME race condition with upload
                            liftIO $ deleteVersion key version
                            put $ CleanupState
                                    { currentKey = key
                                    , storedVersion = noVersion
                                    , seenStoredVersion = True
                                    }

    let baseState = CleanupState
            { currentKey = T.empty
            , storedVersion = noVersion
            , seenStoredVersion = False
            }
    runResourceT $ flip evalStateT baseState $ runConduit $ awsIteratedList'
            (\r -> readResponseIO =<< (lift $ aws ciConfig defServiceConfig ciManager r))
            command
        .| mapM_C checkFile
    logInfo "#S3CLEANUP_END"
    where
    deleteVersion key version = do
        let delCommand = deleteObjectVersion ciBucket key version
        noticeM s3LoggerName $ "#S3DELETEVERSION #file " ++ show key
            ++ " #version " ++ show version
        void $ memoryAws ciConfig defServiceConfig ciManager delCommand
    logInfo :: MonadIO m => String -> m ()
    logInfo = liftIO . infoM s3LoggerName
