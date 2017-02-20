module PrivateCloud.Aws.Logging where

import Blaze.ByteString.Builder
import Control.Monad.IO.Class
import Control.Monad.Trans.AWS
import Data.Text.Lazy
import Data.Text.Lazy.Encoding
import Data.Text.Encoding.Error
import System.Log.Logger

awsLoggerName :: String
awsLoggerName = "PrivateCloud.AWS"

awsLogDebug :: MonadIO m => String -> m ()
awsLogDebug = liftIO . debugM awsLoggerName

awsLogInfo :: MonadIO m => String -> m ()
awsLogInfo = liftIO . infoM awsLoggerName

awsLogNotice :: MonadIO m => String -> m ()
awsLogNotice = liftIO . noticeM awsLoggerName

awsLogError :: MonadIO m => String -> m ()
awsLogError = liftIO . errorM awsLoggerName

awsLogCritical :: MonadIO m => String -> m ()
awsLogCritical = liftIO . criticalM awsLoggerName

builder2string :: Builder -> String
builder2string = unpack . decodeUtf8With lenientDecode . toLazyByteString

amazonkaLogger :: LogLevel -> Builder -> IO ()
amazonkaLogger Trace = awsLogDebug . builder2string
amazonkaLogger Debug = awsLogInfo . builder2string
amazonkaLogger Info = awsLogNotice . builder2string
amazonkaLogger Error = awsLogError . builder2string
