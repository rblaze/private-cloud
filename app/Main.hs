module Main where

import Control.Concurrent
import Control.Monad
import System.FilePath
import System.Log.Logger
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
        let loop = do
                localFiles <- unrollTreeFiles <$> makeTree rootDir
                dbFiles <- getFileList conn
                diff <- getLocalChanges rootDir localFiles dbFiles

                forM_ diff $ \(f, i) -> case i of
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

                threadDelay 10000000
                loop
        loop
