module PrivateCloud.ServiceConfig where

import Aws.Aws
import Aws.S3.Core
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS
import qualified Data.Text as T

import PrivateCloud.Aws

data ServiceConfig = ServiceConfig
    { scConfig :: Configuration
    , scManager :: Manager
    , scDomain :: T.Text
    , scBucket :: Bucket
    , scRoot :: FilePath
    }

withServiceConfig :: FilePath -> String -> (ServiceConfig -> IO a) -> IO a
withServiceConfig root cloudid f = do
    manager <- newManager tlsManagerSettings
    config <- baseConfiguration
    let cloudname = T.pack $ "privatecloud-" ++ cloudid
    f ServiceConfig
        { scConfig = config { logger = awsLogger }
        , scManager = manager
        , scDomain = cloudname
        , scBucket = cloudname
        , scRoot = root
        }
