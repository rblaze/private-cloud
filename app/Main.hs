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
import PrivateCloud.Aws.S3
import PrivateCloud.Aws.SimpleDb
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
                localFiles <- unrollTreeFiles <$> makeTree rootDir
                dbFiles <- getFileList conn
                localDiff <- getLocalChanges rootDir localFiles dbFiles

                forM_ localDiff $ \(f, i) -> case i of
                    ContentChange info -> do
                        body <- BL.readFile (rootDir </> f)
                        uploadFile config f body
                        uploadFileInfo config f info
                        putFileInfo conn f info
                    MetadataOnlyChange info -> do
                        uploadFileInfo config f info
                        putFileInfo conn f info
                    Deleted -> do
                        deleteFile config f
                        removeFileInfo config f
                        deleteFileInfo conn f

                newDbFiles <- getFileList conn
                serverFiles <- getServerFiles config
                serverDiff <- getServerChanges newDbFiles serverFiles
                forM_ serverDiff $ \(f, i) -> case i of
                    ContentChange info -> do
                        body <- downloadFile config f
                        createDirectoryIfMissing True (dropFileName $ rootDir </> f)
                        BL.writeFile (rootDir </> f) body
                        setFileTimes (rootDir </> f) (fiModTime info) (fiModTime info)
                        putFileInfo conn f info
                    MetadataOnlyChange info -> do
                        setFileTimes (rootDir </> f) (fiModTime info) (fiModTime info)
                        putFileInfo conn f info
                    Deleted -> do
                        removeFile (rootDir </> f)
                        deleteFileInfo conn f

                threadDelay 10000000
