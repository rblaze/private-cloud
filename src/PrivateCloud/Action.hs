{-# Language LambdaCase, RecordWildCards, TypeFamilies #-}
module PrivateCloud.Action
    ( syncAllChanges
    , syncRecentChanges
    ) where

import Control.Monad
import Control.Monad.IO.Class
import System.Directory
import System.FilePath

import PrivateCloud.CloudProvider
import PrivateCloud.DirTree
import PrivateCloud.FileInfo
import PrivateCloud.LocalDb
import PrivateCloud.Monad
import PrivateCloud.Sync

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
    patterns <- exclusions
    localFiles <- unrollTreeFiles patterns <$> liftIO (makeTree root)
    dbFiles <- getFileList
    updates <- getUpdates localFiles dbFiles
    -- XXX remove debugging
    liftIO $ print updates

    forM_ updates $ \case
        ResolveConflict{..} ->
            error "Aaaaaaa!!!!!!"
        UpdateCloudFile{..} -> do
            let path = root </> entry2path faFilename
            (version, len, hash) <- runCloud ctx $ uploadFile faFilename path
            let cloudinfo = CloudFileInfo
                    { cfHash = hash
                    , cfModTime = lfModTime faLocalInfo
                    , cfLength = len
                    , cfVersion = version
                    }
            runCloud ctx $ uploadFileInfo faFilename cloudinfo
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
            runCloud ctx $ uploadFileMetadata faFilename dbinfo
            putFileInfo faFilename dbinfo
        DeleteCloudFile{..} -> do
            -- deleteFile faFilename
            runCloud ctx $ uploadDeleteMarker faFilename
            deleteFileInfo faFilename
        UpdateLocalFile{..} -> do
            let path = root </> entry2path faFilename
            liftIO $ createDirectoryIfMissing True (dropFileName path)
            runCloud ctx $ downloadFile faFilename (cfVersion faCloudInfo) path
            liftIO $ setModificationTime path (ts2utc $ cfModTime faCloudInfo)
            putFileInfo faFilename
                DbFileInfo
                    { dfHash = cfHash faCloudInfo
                    , dfLength = cfLength faCloudInfo
                    , dfModTime = cfModTime faCloudInfo
                    }
        UpdateLocalMetadata{..} -> do
            liftIO $ setModificationTime (root </> entry2path faFilename)
                (ts2utc $ cfModTime faCloudInfo)
            putFileInfo faFilename
                DbFileInfo
                    { dfHash = cfHash faCloudInfo
                    , dfLength = cfLength faCloudInfo
                    , dfModTime = cfModTime faCloudInfo
                    }
        DeleteLocalFile{..} -> do
            liftIO $ removeFile (root </> entry2path faFilename)
            deleteFileInfo faFilename
