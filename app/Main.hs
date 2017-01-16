module Main where

import System.Environment

import PrivateCloud.Aws
import PrivateCloud.DirTree

main :: IO ()
main = do
    [root] <- getArgs
    config <- defaultCloudInfo
    baseList <- unrollTreeFiles <$> makeTree root
    uploadFileInfo config root (fst $ head baseList)
