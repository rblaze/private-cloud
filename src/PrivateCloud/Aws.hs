{-# Language OverloadedStrings #-}
module PrivateCloud.Aws where

import Aws.Aws
import Aws.S3.Core
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS
import qualified Data.Text as T
import qualified System.Log.Logger as L

data CloudInfo = CloudInfo
    { ciConfig :: Configuration
    , ciManager :: Manager
    , ciDomain :: T.Text
    , ciBucket :: Bucket
    }

awsLoggerName :: String
awsLoggerName = "PrivateCloud.AWS"

awsLogger :: Logger
awsLogger Debug   = L.debugM awsLoggerName . T.unpack
awsLogger Info    = L.infoM awsLoggerName . T.unpack
awsLogger Warning = L.warningM awsLoggerName . T.unpack
awsLogger Error   = L.errorM awsLoggerName . T.unpack

defaultCloudInfo :: IO CloudInfo
defaultCloudInfo = do
    manager <- newManager tlsManagerSettings
    config <- baseConfiguration
    return CloudInfo
        { ciConfig = config { logger = awsLogger }
        , ciManager = manager
        , ciDomain = "privatecloud"
        , ciBucket = "blaze-privatecloud"
        }
