{-# Language LambdaCase, RecordWildCards #-}
module PrivateCloud.Action
    ( syncAllChanges
    , syncRecentChanges
    ) where

import Control.Monad
import Control.Monad.IO.Class
import System.Directory
import System.FilePath
import qualified Data.ByteString.Lazy as BL

import PrivateCloud.Aws.S3
import PrivateCloud.Aws.SimpleDb
import PrivateCloud.DirTree
import PrivateCloud.FileInfo
import PrivateCloud.LocalDb
import PrivateCloud.Monad
import PrivateCloud.ServiceConfig
import PrivateCloud.Sync

syncAllChanges :: MonadIO m => ServiceConfig -> PrivateCloudT m ()
syncAllChanges config =
    syncChanges config $ \localFiles dbFiles -> do
        serverFiles <- getAllServerFiles config
        getAllFileChanges config localFiles dbFiles serverFiles

syncRecentChanges :: MonadIO m => ServiceConfig -> PrivateCloudT m ()
syncRecentChanges config =
    syncChanges config $ \localFiles dbFiles -> do
        serverFiles <- getRecentServerFiles config
        getRecentFileChanges config localFiles dbFiles serverFiles

syncChanges :: MonadIO m => ServiceConfig -> (LocalFileList -> DbFileList -> IO [FileAction]) -> PrivateCloudT m ()
syncChanges config@ServiceConfig{..} getUpdates = do
    patterns <- exclusions
    localFiles <- unrollTreeFiles patterns <$> liftIO (makeTree scRoot)
    dbFiles <- getFileList
    updates <- liftIO $ getUpdates localFiles dbFiles
    -- XXX remove debugging
    liftIO $ print updates

    forM_ updates $ \case
        ResolveConflict{..} ->
            error "Aaaaaaa!!!!!!"
        UpdateCloudFile{..} -> do
            let path = scRoot </> entry2path faFilename
            (version, len, hash) <-liftIO $ uploadFile config faFilename path
            let cloudinfo = CloudFileInfo
                    { cfHash = hash
                    , cfModTime = lfModTime faLocalInfo
                    , cfLength = len
                    , cfVersion = version
                    }
            liftIO $ uploadFileInfo config faFilename cloudinfo
            let dbinfo = DbFileInfo
                    { dfHash = hash
                    , dfModTime = lfModTime faLocalInfo
                    , dfLength = len
                    }
            putFileInfo faFilename dbinfo
        UpdateCloudMetadata{..} -> do
            let dbinfo = DbFileInfo
                    { dfHash = faExpectedHash
                    , dfModTime = lfModTime faLocalInfo
                    , dfLength = lfLength faLocalInfo
                    }
            liftIO $ uploadFileMetadata config faFilename dbinfo
            putFileInfo faFilename dbinfo
        DeleteCloudFile{..} -> do
            liftIO $ deleteFile config faFilename
            liftIO $ uploadDeleteMarker config faFilename
            deleteFileInfo faFilename
        UpdateLocalFile{..} -> do
            let path = scRoot </> entry2path faFilename
            body <- liftIO $ downloadFile config faFilename (cfVersion faCloudInfo)
            liftIO $ createDirectoryIfMissing True (dropFileName path)
            liftIO $ BL.writeFile path body
            liftIO $ setModificationTime path (ts2utc $ cfModTime faCloudInfo)
            putFileInfo faFilename
                DbFileInfo
                    { dfHash = cfHash faCloudInfo
                    , dfLength = cfLength faCloudInfo
                    , dfModTime = cfModTime faCloudInfo
                    }
        UpdateLocalMetadata{..} -> do
            liftIO $ setModificationTime (scRoot </> entry2path faFilename)
                (ts2utc $ cfModTime faCloudInfo)
            putFileInfo faFilename
                DbFileInfo
                    { dfHash = cfHash faCloudInfo
                    , dfLength = cfLength faCloudInfo
                    , dfModTime = cfModTime faCloudInfo
                    }
        DeleteLocalFile{..} -> do
            liftIO $ removeFile (scRoot </> entry2path faFilename)
            deleteFileInfo faFilename
