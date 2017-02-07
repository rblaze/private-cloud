{-# Language LambdaCase, RecordWildCards #-}
module PrivateCloud.Action
    ( syncAllChanges
    , syncRecentChanges
    ) where

import Control.Monad
import System.Directory
import System.FilePath
import System.Posix.Files
import qualified Data.ByteString.Lazy as BL

import PrivateCloud.Aws
import PrivateCloud.Aws.S3
import PrivateCloud.Aws.SimpleDb
import PrivateCloud.DirTree
import PrivateCloud.FileInfo
import PrivateCloud.LocalDb
import PrivateCloud.Sync

syncAllChanges :: FilePath -> DbInfo -> CloudInfo -> IO ()
syncAllChanges root conn config =
    syncChanges root conn config $ \localFiles dbFiles -> do
        serverFiles <- getAllServerFiles config
        print serverFiles
        getAllFileChanges root localFiles dbFiles serverFiles

syncRecentChanges :: FilePath -> DbInfo -> CloudInfo -> IO ()
syncRecentChanges root conn config =
    syncChanges root conn config $ \localFiles dbFiles -> do
        serverFiles <- getRecentServerFiles config
        print serverFiles
        getRecentFileChanges root localFiles dbFiles serverFiles

syncChanges :: FilePath -> DbInfo -> CloudInfo -> (LocalFileList -> DbFileList -> IO [FileAction]) -> IO ()
syncChanges root conn config getUpdates = do
    localFiles <- unrollTreeFiles <$> makeTree root
    dbFiles <- getFileList conn
    updates <- getUpdates localFiles dbFiles
    print updates

    forM_ updates $ \case
        ResolveConflict{..} ->
            error "Aaaaaaa!!!!!!"
        UpdateCloudFile{..} -> do
            let path = root </> entry2path faFilename
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
            putFileInfo conn faFilename dbinfo
        UpdateCloudMetadata{..} -> do
            let dbinfo = DbFileInfo
                    { dfHash = faExpectedHash
                    , dfModTime = lfModTime faLocalInfo
                    , dfLength = lfLength faLocalInfo
                    }
            uploadFileMetadata config faFilename dbinfo
            putFileInfo conn faFilename dbinfo
        DeleteCloudFile{..} -> do
            deleteFile config faFilename
            uploadDeleteMarker config faFilename
            deleteFileInfo conn faFilename
        UpdateLocalFile{..} -> do
            let path = root </> entry2path faFilename
            body <- downloadFile config faFilename (cfVersion faCloudInfo)
            createDirectoryIfMissing True (dropFileName path)
            BL.writeFile path body
            setFileTimes path (ts2epoch $ cfModTime faCloudInfo) (ts2epoch $ cfModTime faCloudInfo)
            putFileInfo conn faFilename
                DbFileInfo
                    { dfHash = cfHash faCloudInfo
                    , dfLength = cfLength faCloudInfo
                    , dfModTime = cfModTime faCloudInfo
                    }
        UpdateLocalMetadata{..} -> do
            setFileTimes (root </> entry2path faFilename)
                (ts2epoch $ cfModTime faCloudInfo)
                (ts2epoch $ cfModTime faCloudInfo)
            putFileInfo conn faFilename
                DbFileInfo
                    { dfHash = cfHash faCloudInfo
                    , dfLength = cfLength faCloudInfo
                    , dfModTime = cfModTime faCloudInfo
                    }
        DeleteLocalFile{..} -> do
            removeFile (root </> entry2path faFilename)
            deleteFileInfo conn faFilename
