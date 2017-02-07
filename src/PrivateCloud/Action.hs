{-# Language LambdaCase, RecordWildCards #-}
module PrivateCloud.Action
    ( syncAllChanges
    , syncRecentChanges
    ) where

import Control.Monad
import System.Directory
import System.FilePath
import System.FilePath.Glob
import qualified Data.ByteString.Lazy as BL

import PrivateCloud.Aws
import PrivateCloud.Aws.S3
import PrivateCloud.Aws.SimpleDb
import PrivateCloud.DirTree
import PrivateCloud.FileInfo
import PrivateCloud.LocalDb
import PrivateCloud.Sync

syncAllChanges :: FilePath -> [Pattern] -> DbInfo -> CloudInfo -> IO ()
syncAllChanges root exclusions conn config =
    syncChanges root exclusions conn config $ \localFiles dbFiles -> do
        serverFiles <- getAllServerFiles config
        getAllFileChanges root localFiles dbFiles serverFiles

syncRecentChanges :: FilePath -> [Pattern] -> DbInfo -> CloudInfo -> IO ()
syncRecentChanges root exclusions conn config =
    syncChanges root exclusions conn config $ \localFiles dbFiles -> do
        serverFiles <- getRecentServerFiles config
        getRecentFileChanges root localFiles dbFiles serverFiles

syncChanges :: FilePath -> [Pattern] -> DbInfo -> CloudInfo -> (LocalFileList -> DbFileList -> IO [FileAction]) -> IO ()
syncChanges root exclusions conn config getUpdates = do
    localFiles <- unrollTreeFiles exclusions <$> makeTree root
    dbFiles <- getFileList conn
    updates <- getUpdates localFiles dbFiles
    -- XXX remove debugging
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
            setModificationTime path (ts2utc $ cfModTime faCloudInfo)
            putFileInfo conn faFilename
                DbFileInfo
                    { dfHash = cfHash faCloudInfo
                    , dfLength = cfLength faCloudInfo
                    , dfModTime = cfModTime faCloudInfo
                    }
        UpdateLocalMetadata{..} -> do
            setModificationTime (root </> entry2path faFilename)
                (ts2utc $ cfModTime faCloudInfo)
            putFileInfo conn faFilename
                DbFileInfo
                    { dfHash = cfHash faCloudInfo
                    , dfLength = cfLength faCloudInfo
                    , dfModTime = cfModTime faCloudInfo
                    }
        DeleteLocalFile{..} -> do
            removeFile (root </> entry2path faFilename)
            deleteFileInfo conn faFilename