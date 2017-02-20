module PrivateCloud.Aws.Logging where

import Control.Monad.IO.Class
import qualified System.Log.Logger as L

awsLoggerName :: String
awsLoggerName = "PrivateCloud.AWS"

awsLogInfo :: MonadIO m => String -> m ()
awsLogInfo = liftIO . L.infoM awsLoggerName

awsLogNotice :: MonadIO m => String -> m ()
awsLogNotice = liftIO . L.noticeM awsLoggerName

awsLogCritical :: MonadIO m => String -> m ()
awsLogCritical = liftIO . L.criticalM awsLoggerName
