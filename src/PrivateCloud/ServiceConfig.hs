module PrivateCloud.ServiceConfig where

import Aws.Aws
import Aws.S3.Core
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS
import System.FilePath.Glob
import qualified Data.Text as T

import PrivateCloud.Aws

data ServiceConfig = ServiceConfig
    { scConfig :: Configuration
    , scManager :: Manager
    , scDomain :: T.Text
    , scBucket :: Bucket
    , scRoot :: FilePath
    , scExclusions :: [Pattern]
    }

withServiceConfig :: FilePath -> String -> [Pattern] -> (ServiceConfig -> IO a) -> IO a
withServiceConfig root cloudid exclusions f = do
    manager <- newManager tlsManagerSettings
    config <- baseConfiguration
    let cloudname = T.pack $ "privatecloud-" ++ cloudid
    f ServiceConfig
        { scConfig = config { logger = awsLogger }
        , scManager = manager
        , scDomain = cloudname
        , scBucket = cloudname
        , scRoot = root
        , scExclusions = exclusions
        }
