module PrivateCloud.ServiceConfig where

import Aws.Aws
import Aws.S3.Core
import Database.SQLite.Simple
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS
import System.FilePath
import System.FilePath.Glob
import qualified Data.Text as T

import PrivateCloud.Aws

data ServiceConfig = ServiceConfig
    { scConfig :: Configuration
    , scManager :: Manager
    , scDomain :: T.Text
    , scBucket :: Bucket
    , scConnection :: Connection
    , scRoot :: FilePath
    , scExclusions :: [Pattern]
    }

dbName :: FilePath
dbName = ".privatecloud"

withServiceConfig :: FilePath -> String -> [Pattern] -> (ServiceConfig -> IO a) -> IO a
withServiceConfig root cloudid exclusions f = do
    manager <- newManager tlsManagerSettings
    config <- baseConfiguration
    let dbFileName = root </> dbName
    withConnection dbFileName $ \conn -> do
        let cloudname = T.pack $ "privatecloud-" ++ cloudid
        f ServiceConfig
            { scConfig = config { logger = awsLogger }
            , scManager = manager
            , scDomain = cloudname
            , scBucket = cloudname
            , scConnection = conn
            , scRoot = root
            , scExclusions = exclusions
            }
