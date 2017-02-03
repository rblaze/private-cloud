{-# Language LambdaCase, RecordWildCards #-}
module Main where

import Control.Concurrent
import Control.Exception.Safe
import Control.Monad
import System.Directory
import System.FilePath
import System.Log.Logger
import System.Posix.Files
import qualified Data.ByteString.Lazy as BL

import PrivateCloud.Aws
--import PrivateCloud.Aws.Cleanup
import PrivateCloud.Aws.S3
import PrivateCloud.Aws.SimpleDb
import PrivateCloud.Crypto
import PrivateCloud.DirTree
import PrivateCloud.FileInfo
import PrivateCloud.LocalDb
import PrivateCloud.Sync

import Options

mainLoggerName :: String
mainLoggerName = "PrivateCloud"

main :: IO ()
main = do
    options <- getOptions
    config <- defaultCloudInfo

    updateGlobalLogger mainLoggerName (setLevel (loglevel options))

    let rootDir = root options
    noticeM mainLoggerName $ "#START #root " ++ rootDir

    withDatabase (rootDir </> dbName) $ \conn -> do
        noticeM mainLoggerName "#DBOPEN"

        forever $ handleAny
            (\e -> errorM mainLoggerName $ "#EXCEPTION #msg " ++ show e) $
            do
                syncChanges rootDir conn config
                -- FIXME run cleanup rarely
--                deleteOldVersions config
--                deleteOldDbRecords config
                threadDelay 10000000

syncChanges :: FilePath -> DbInfo -> CloudInfo -> IO ()
syncChanges rootDir conn config = do
    localFiles <- unrollTreeFiles <$> makeTree rootDir
    dbFiles <- getFileList conn
    serverFiles <- getServerFiles config

    updates <- getFileChanges rootDir localFiles dbFiles serverFiles
    print updates

    forM_ updates $ \case
        ResolveConflict{..} ->
            error "Aaaaaaa!!!!!!"
        UpdateCloudFile{..} -> do
            let path = rootDir </> entry2path faFilename
            body <- BL.readFile path
            version <- uploadFile config faFilename body
            -- FIXME return CloudFileInfo from uploadFile
            hash <- getFileHash path
            let cloudinfo = CloudFileInfo
                    { cfHash = hash
                    , cfModTime = lfModTime faLocalInfo
                    , cfLength = lfLength faLocalInfo
                    , cfVersion = version
                    }
            uploadFileInfo config faFilename cloudinfo
            let dbinfo = DbFileInfo
                    { dfHash = hash
                    , dfModTime = lfModTime faLocalInfo
                    , dfLength = lfLength faLocalInfo
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
            let path = rootDir </> entry2path faFilename
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
            setFileTimes (rootDir </> entry2path faFilename)
                (ts2epoch $ cfModTime faCloudInfo)
                (ts2epoch $ cfModTime faCloudInfo)
            putFileInfo conn faFilename
                DbFileInfo
                    { dfHash = cfHash faCloudInfo
                    , dfLength = cfLength faCloudInfo
                    , dfModTime = cfModTime faCloudInfo
                    }
        DeleteLocalFile{..} -> do
            removeFile (rootDir </> entry2path faFilename)
            deleteFileInfo conn faFilename
