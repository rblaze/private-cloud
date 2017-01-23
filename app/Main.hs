module Main where

import Control.Concurrent
import Control.Monad
import System.Environment
import System.FilePath

import PrivateCloud.DirTree
import PrivateCloud.FileInfo
import PrivateCloud.LocalDb
import PrivateCloud.Sync

main :: IO ()
main = do
    [root] <- getArgs
    withDatabase (root </> dbName) $ \conn -> do
        let loop = do
                localFiles <- unrollTreeFiles <$> makeTree root
                dbFiles <- getFileList conn
                diff <- getLocalChanges root localFiles dbFiles
                print diff

                forM_ diff $ \(f, i) -> case i of
                    Just v -> putFileInfo conn f v
                    Nothing -> deleteFileInfo conn f

                threadDelay 10000000
                loop
        loop
