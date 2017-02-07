{-# Language LambdaCase, RecordWildCards #-}
module Main where

import Control.Concurrent
import Control.Exception.Safe
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Data.Time.Clock
import System.FilePath
import System.Log.Logger

import PrivateCloud.Action
import PrivateCloud.Aws
import PrivateCloud.Aws.Cleanup
import PrivateCloud.FileInfo
import PrivateCloud.LocalDb

import Options

mainLoggerName :: String
mainLoggerName = "PrivateCloud"

main :: IO ()
main = do
    options <- getOptions
    config <- defaultCloudInfo

    print options
    let Options{..} = options
    let fullSyncDelay = fromIntegral (fullSyncInterval * 60)
    let cleanupDelay = fromIntegral (cleanupInterval * 60)

    updateGlobalLogger mainLoggerName (setLevel loglevel)

    noticeM mainLoggerName $ "#START #root " ++ root

    withDatabase (root </> dbName) $ \conn -> do
        noticeM mainLoggerName "#DBOPEN"

        syncAllChanges root conn config
        startTime <- getCurrentTime

        void $ flip runStateT (startTime, startTime) $ forever $ handleAny
            (\e -> lift $ errorM mainLoggerName $ "#EXCEPTION #msg " ++ show e) $
            do
                lift $ threadDelay (60000000 * fromIntegral syncInterval)
                currentTime <- lift $ getCurrentTime
                sinceLastFullSync <- diffUTCTime currentTime <$> gets fst

                if sinceLastFullSync > fullSyncDelay
                    then do
                        lift $ syncAllChanges root conn config
                        modify $ \(_, lastTime) -> (currentTime, lastTime)
                    else lift $ do
                        noticeM mainLoggerName $ "#TIMER #tillNextFullSync " ++ show (fullSyncDelay - sinceLastFullSync)
                        syncRecentChanges root conn config

                sinceLastCleanup <- diffUTCTime currentTime <$> gets snd
                if sinceLastCleanup > cleanupDelay
                    then do
                        lift $ deleteOldVersions config
                        lift $ deleteOldDbRecords config
                        modify $ \(lastTime, _) -> (lastTime, currentTime)
                    else
                        lift $ noticeM mainLoggerName $ "#TIMER #tillNextCleanup " ++ show (cleanupDelay - sinceLastCleanup)
