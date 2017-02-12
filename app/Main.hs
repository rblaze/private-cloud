{-# Language LambdaCase, RecordWildCards #-}
module Main where

import Control.Concurrent
import Control.Exception.Safe
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Data.Time.Clock
import System.Exit
import System.FilePath.Glob
import System.Log.Logger

import PrivateCloud.Action
import PrivateCloud.Aws.Cleanup
import PrivateCloud.LocalDb
import PrivateCloud.ServiceConfig

import Options

mainLoggerName :: String
mainLoggerName = "PrivateCloud"

main :: IO ()
main = do
    options <- getOptions
    print options

    let Options{..} = options
    let fullSyncDelay = fromIntegral (fullSyncInterval * 60)
    let cleanupDelay = fromIntegral (cleanupInterval * 60)

    updateGlobalLogger mainLoggerName (setLevel loglevel)

    noticeM mainLoggerName $ "#START #root " ++ root

    exclusions <- forM (dbName : patterns) $ \pat -> do
        case simplify <$> tryCompileWith compPosix pat of
            Left errmsg -> do
                errorM mainLoggerName $ "#EXCLUSION_ERROR #pattern " ++ show pat
                    ++ " #msg " ++ errmsg
                exitFailure
            Right pattern -> do
                infoM mainLoggerName $ "#EXCLUSION #pattern " ++ show pat
                return pattern

    withServiceConfig root "devtest" exclusions $ \config -> do
        noticeM mainLoggerName "#DBOPEN"
        initDatabase config

        syncAllChanges config
        startTime <- getCurrentTime

        void $ flip runStateT (startTime, startTime) $ forever $ handleAny
            (\e -> lift $ errorM mainLoggerName $ "#EXCEPTION #msg " ++ show e) $
            do
                lift $ threadDelay (60000000 * fromIntegral syncInterval)
                currentTime <- lift $ getCurrentTime
                sinceLastFullSync <- diffUTCTime currentTime <$> gets fst

                if sinceLastFullSync > fullSyncDelay
                    then do
                        lift $ syncAllChanges config
                        modify $ \(_, lastTime) -> (currentTime, lastTime)
                    else lift $ do
                        noticeM mainLoggerName $ "#TIMER #tillNextFullSync " ++ show (fullSyncDelay - sinceLastFullSync)
                        syncRecentChanges config

                sinceLastCleanup <- diffUTCTime currentTime <$> gets snd
                if sinceLastCleanup > cleanupDelay
                    then do
                        lift $ deleteOldVersions config
                        lift $ deleteOldDbRecords config
                        modify $ \(lastTime, _) -> (lastTime, currentTime)
                    else
                        lift $ noticeM mainLoggerName $ "#TIMER #tillNextCleanup " ++ show (cleanupDelay - sinceLastCleanup)
