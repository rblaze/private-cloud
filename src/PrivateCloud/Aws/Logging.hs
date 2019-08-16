module PrivateCloud.Aws.Logging
    ( awsLogger
    , sdbLogInfo
    , sdbLogCritical
    , s3LogInfo
    ) where

import Aws.Aws
import Control.Monad.IO.Class
import System.Log.Logger
import qualified Data.Text as T

awsLoggerName :: String
awsLoggerName = "PrivateCloud.AWS"

awsLogger :: Aws.Aws.Logger
awsLogger Debug = debugM awsLoggerName . T.unpack
awsLogger Info = infoM awsLoggerName . T.unpack
awsLogger Warning = warningM awsLoggerName . T.unpack
awsLogger Error = errorM awsLoggerName . T.unpack

sdbLoggerName :: String
sdbLoggerName = awsLoggerName <> ".SimpleDb"

sdbLogInfo :: MonadIO m => String -> m ()
sdbLogInfo = liftIO . infoM sdbLoggerName

sdbLogCritical :: MonadIO m => String -> m ()
sdbLogCritical = liftIO . criticalM sdbLoggerName

s3LoggerName :: String
s3LoggerName = awsLoggerName <> ".S3"

s3LogInfo :: MonadIO m => String -> m ()
s3LogInfo = liftIO . infoM s3LoggerName
