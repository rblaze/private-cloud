module PrivateCloud.Cloud.EventLog where

import Control.Monad.IO.Class
import System.Log.Logger

-- | Logger for user-facing events. Should be exposed to UX by default.
eventLoggerName :: String
eventLoggerName = "PrivateCloud.Events"

logEventNotice :: MonadIO m => String -> m ()
logEventNotice = liftIO . noticeM eventLoggerName

logEventWarning :: MonadIO m => String -> m ()
logEventWarning = liftIO . warningM eventLoggerName

logEventError :: MonadIO m => String -> m ()
logEventError = liftIO . errorM eventLoggerName
