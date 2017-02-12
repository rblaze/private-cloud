{-# Language OverloadedStrings #-}
module PrivateCloud.Aws where

import Aws.Aws
import Control.Exception.Safe
import Control.Monad.IO.Class
import qualified Data.Text as T
import qualified System.Log.Logger as L

awsLoggerName :: String
awsLoggerName = "PrivateCloud.AWS"

awsLogInfo :: MonadIO m => String -> m ()
awsLogInfo = liftIO . L.infoM awsLoggerName

awsLogNotice :: MonadIO m => String -> m ()
awsLogNotice = liftIO . L.noticeM awsLoggerName

awsLogCritical :: MonadIO m => String -> m ()
awsLogCritical = liftIO . L.criticalM awsLoggerName

awsLogger :: Logger
awsLogger Debug   = L.debugM awsLoggerName . T.unpack
awsLogger Info    = L.infoM awsLoggerName . T.unpack
awsLogger Warning = L.warningM awsLoggerName . T.unpack
awsLogger Error   = L.errorM awsLoggerName . T.unpack

data AwsException = AwsException String
    deriving Show

instance Exception AwsException
