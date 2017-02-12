{-# Language LambdaCase, RecordWildCards #-}
module PrivateCloud.Action
    ( syncAllChanges
    , syncRecentChanges
    ) where

import Control.Monad
import System.Directory
import System.FilePath
import qualified Data.ByteString.Lazy as BL

import PrivateCloud.Aws.S3
import PrivateCloud.Aws.SimpleDb
import PrivateCloud.DirTree
import PrivateCloud.FileInfo
import PrivateCloud.LocalDb
import PrivateCloud.ServiceConfig
import PrivateCloud.Sync

syncAllChanges :: ServiceConfig -> IO ()
syncAllChanges config =
    syncChanges config $ \localFiles dbFiles -> do
        serverFiles <- getAllServerFiles config
        getAllFileChanges config localFiles dbFiles serverFiles

syncRecentChanges :: ServiceConfig -> IO ()
syncRecentChanges config =
    syncChanges config $ \localFiles dbFiles -> do
        serverFiles <- getRecentServerFiles config
        getRecentFileChanges config localFiles dbFiles serverFiles

syncChanges :: ServiceConfig -> (LocalFileList -> DbFileList -> IO [FileAction]) -> IO ()
syncChanges config@ServiceConfig{..} getUpdates = do
    localFiles <- unrollTreeFiles scExclusions <$> makeTree scRoot
    dbFiles <- getFileList config
    updates <- getUpdates localFiles dbFiles
    -- XXX remove debugging
    print updates

    forM_ updates $ \case
        ResolveConflict{..} ->
            error "Aaaaaaa!!!!!!"
        UpdateCloudFile{..} -> do
            let path = scRoot </> entry2path faFilename
            (version, len, hash) <- uploadFile config faFilename path
            let cloudinfo = CloudFileInfo
                    { cfHash = hash
                    , cfModTime = lfModTime faLocalInfo
                    , cfLength = len
                    , cfVersion = version
                    }
            uploadFileInfo config faFilename cloudinfo
            let dbinfo = DbFileInfo
                    { dfHash = hash
                    , dfModTime = lfModTime faLocalInfo
                    , dfLength = len
                    }
            putFileInfo config faFilename dbinfo
        UpdateCloudMetadata{..} -> do
            let dbinfo = DbFileInfo
                    { dfHash = faExpectedHash
                    , dfModTime = lfModTime faLocalInfo
                    , dfLength = lfLength faLocalInfo
                    }
            uploadFileMetadata config faFilename dbinfo
            putFileInfo config faFilename dbinfo
        DeleteCloudFile{..} -> do
            deleteFile config faFilename
            uploadDeleteMarker config faFilename
            deleteFileInfo config faFilename
        UpdateLocalFile{..} -> do
            let path = scRoot </> entry2path faFilename
            body <- downloadFile config faFilename (cfVersion faCloudInfo)
            createDirectoryIfMissing True (dropFileName path)
            BL.writeFile path body
            setModificationTime path (ts2utc $ cfModTime faCloudInfo)
            putFileInfo config faFilename
                DbFileInfo
                    { dfHash = cfHash faCloudInfo
                    , dfLength = cfLength faCloudInfo
                    , dfModTime = cfModTime faCloudInfo
                    }
        UpdateLocalMetadata{..} -> do
            setModificationTime (scRoot </> entry2path faFilename)
                (ts2utc $ cfModTime faCloudInfo)
            putFileInfo config faFilename
                DbFileInfo
                    { dfHash = cfHash faCloudInfo
                    , dfLength = cfLength faCloudInfo
                    , dfModTime = cfModTime faCloudInfo
                    }
        DeleteLocalFile{..} -> do
            removeFile (scRoot </> entry2path faFilename)
            deleteFileInfo config faFilename
