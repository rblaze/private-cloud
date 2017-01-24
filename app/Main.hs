module Main where

import Control.Concurrent
import Control.Monad
import System.FilePath
import System.Log.Logger

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
                    Just v -> putFileInfo conn f v
                    Nothing -> deleteFileInfo conn f

                threadDelay 10000000
                loop
        loop
