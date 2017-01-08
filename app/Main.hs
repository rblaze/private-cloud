module Main where

import Control.Concurrent.STM
import System.INotify
import PrivateCloud.FileWatch

main :: IO ()
main = withINotify $ \notify -> do
    queue <- atomically $ newTQueue
    watchTree notify "/tmp/a" queue
    loop queue
    where
    loop queue = do
        event <- atomically $ readTQueue queue
        case event of
            Gone -> return ()
            _ -> do
                putStrLn $ "-- " ++ show event
                loop queue
