{-# Language OverloadedStrings #-}
module PrivateCloud.Aws where

import Aws.Aws
import Aws.S3.Core
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS
import qualified Data.Text as T

data CloudInfo = CloudInfo
    { ciConfig :: Configuration
    , ciManager :: Manager
    , ciDomain :: T.Text
    , ciBucket :: Bucket
    }

defaultCloudInfo :: IO CloudInfo
defaultCloudInfo = do
    manager <- newManager tlsManagerSettings
    config <- baseConfiguration
    return CloudInfo
        { ciConfig = config
        , ciManager = manager
        , ciDomain = "privatecloud"
        , ciBucket = "blaze-privatecloud"
        }
