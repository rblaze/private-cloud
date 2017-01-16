module Main where

import Aws.Aws
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import System.Environment

import PrivateCloud.Aws
import PrivateCloud.DirTree

main :: IO ()
main = do
    [root] <- getArgs
    config <- baseConfiguration
    baseList <- unrollTreeFiles <$> makeTree root
    manager <- newManager tlsManagerSettings
    uploadFileInfo config manager root (fst $ head baseList)
