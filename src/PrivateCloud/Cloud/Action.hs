{-# Language LambdaCase, RecordWildCards, TypeFamilies #-}
module PrivateCloud.Cloud.Action
    ( syncAllChanges
    , syncRecentChanges
    ) where

import Control.Monad
import Control.Monad.IO.Class
import System.Directory
import System.FilePath

import PrivateCloud.Cloud.DirTree
import PrivateCloud.Cloud.EventLog
import PrivateCloud.Cloud.LocalDb
import PrivateCloud.Cloud.Monad
import PrivateCloud.Cloud.Sync
import PrivateCloud.Provider.Class
import PrivateCloud.Provider.Types

syncAllChanges :: CloudProvider p => PrivateCloud p ()
syncAllChanges =
    syncChanges $ \localFiles dbFiles -> do
        ctx <- context
        serverFiles <- runCloud ctx getAllServerFiles
        getAllFileChanges localFiles dbFiles serverFiles

syncRecentChanges :: CloudProvider p => PrivateCloud p ()
syncRecentChanges =
    syncChanges $ \localFiles dbFiles -> do
        ctx <- context
        serverFiles <- runCloud ctx getRecentServerFiles
        getRecentFileChanges localFiles dbFiles serverFiles

syncChanges :: CloudProvider p => (LocalFileList -> DbFileList -> PrivateCloud p [FileAction]) -> PrivateCloud p ()
syncChanges getUpdates = do
    ctx <- context
    root <- rootDir
    localFiles <- unrollTreeFiles <$> liftIO (makeTree root)
    dbFiles <- getFileList
    updates <- getUpdates localFiles dbFiles

    forM_ updates $ \case
        ResolveConflict{..} -> do
            logEventNotice $ "CONFLICT #file " ++ printEntry faFilename
            error "Aaaaaaa!!!!!!"
        UpdateCloudFile{..} -> do
            logEventNotice $ "UPLOAD_FILE_START #file " ++ printEntry faFilename
            let path = root </> entry2path faFilename
            (version, len, hash) <- runCloud ctx $ uploadFile faFilename path
            logEventNotice $ "UPLOAD_DATA_COMPLETED #file " ++ printEntry faFilename
            let cloudinfo = CloudFileInfo
                    { cfHash = hash
                    , cfModTime = lfModTime faLocalInfo
                    , cfLength = len
                    , cfVersion = version
                    }
            logEventNotice $ "UPLOAD_META_COMPLETED #file " ++ printEntry faFilename
            runCloud ctx $ uploadFileInfo faFilename cloudinfo
            let dbinfo = DbFileInfo
                    { dfHash = hash
                    , dfModTime = lfModTime faLocalInfo
                    , dfLength = len
                    }
            putFileInfo faFilename dbinfo
            logEventNotice $ "UPLOAD_FILE_END #file " ++ printEntry faFilename
        UpdateCloudMetadata{..} -> do
            logEventNotice $ "UPLOAD_META_START #file " ++ printEntry faFilename
            let dbinfo = DbFileInfo
                    { dfHash = faExpectedHash
                    , dfModTime = lfModTime faLocalInfo
                    , dfLength = lfLength faLocalInfo
                    }
            runCloud ctx $ uploadFileMetadata faFilename dbinfo
            logEventNotice $ "UPLOAD_META_COMPLETED #file " ++ printEntry faFilename
            putFileInfo faFilename dbinfo
            logEventNotice $ "UPLOAD_META_END #file " ++ printEntry faFilename
        DeleteCloudFile{..} -> do
            logEventNotice $ "DELETE_CLOUD_START #file " ++ printEntry faFilename
            runCloud ctx $ uploadDeleteMarker faFilename
            logEventNotice $ "DELETE_CLOUD_META_COMPLETED #file " ++ printEntry faFilename
            deleteFileInfo faFilename
            logEventNotice $ "DELETE_CLOUND_END #file " ++ printEntry faFilename
        UpdateLocalFile{..} -> do
            logEventNotice $ "DOWNLOAD_FILE_START #file " ++ printEntry faFilename
            let path = root </> entry2path faFilename
            liftIO $ createDirectoryIfMissing True (dropFileName path)
            runCloud ctx $ downloadFile faFilename (cfVersion faCloudInfo) (cfHash faCloudInfo) path
            logEventNotice $ "DOWNLOAD_DATA_COMPLETED #file " ++ printEntry faFilename
            liftIO $ setModificationTime path (ts2utc $ cfModTime faCloudInfo)
            putFileInfo faFilename
                DbFileInfo
                    { dfHash = cfHash faCloudInfo
                    , dfLength = cfLength faCloudInfo
                    , dfModTime = cfModTime faCloudInfo
                    }
            logEventNotice $ "DOWNLOAD_FILE_END #file " ++ printEntry faFilename
        UpdateLocalMetadata{..} -> do
            logEventNotice $ "DOWNLOAD_META_START #file " ++ printEntry faFilename
            liftIO $ setModificationTime (root </> entry2path faFilename)
                (ts2utc $ cfModTime faCloudInfo)
            putFileInfo faFilename
                DbFileInfo
                    { dfHash = cfHash faCloudInfo
                    , dfLength = cfLength faCloudInfo
                    , dfModTime = cfModTime faCloudInfo
                    }
            logEventNotice $ "DOWNLOAD_META_END #file " ++ printEntry faFilename
        DeleteLocalFile{..} -> do
            logEventNotice $ "DELETE_LOCAL_START #file " ++ printEntry faFilename
            liftIO $ removeFile (root </> entry2path faFilename)
            logEventNotice $ "DELETE_LOCAL_FILE_COMPLETED #file " ++ printEntry faFilename
            deleteFileInfo faFilename
            logEventNotice $ "DELETE_LOCAL_END #file " ++ printEntry faFilename
