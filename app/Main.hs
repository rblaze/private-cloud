module Main where

import System.Environment

import PrivateCloud.Aws
import PrivateCloud.DirTree

main :: IO ()
main = do
    config <- defaultCloudInfo
    serverFiles <- getServerFiles config
    [root] <- getArgs
    localFiles <- unrollTreeFiles <$> makeTree root
    let diff = getChangedFiles serverFiles localFiles 
    print serverFiles
    print "--"
    print localFiles
    print "--"
    print diff
    mapM_ (updateInfo config root) diff
