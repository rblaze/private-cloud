{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module PrivateCloud.Aws.Provider where

import Aws.Aws
import Aws.Core
import Control.Monad.Trans.Reader
import Data.ByteArray
import Data.Tagged
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import System.FilePath.Glob
import qualified Data.Text as T
import qualified Data.ByteString as BS

import PrivateCloud.Aws.Account as AWS
import PrivateCloud.Aws.S3 as S3
import PrivateCloud.Aws.SimpleDb as SDB
import PrivateCloud.Aws.Cleanup
import PrivateCloud.Aws.Logging
import PrivateCloud.Aws.Monad
import PrivateCloud.Provider.Class
import PrivateCloud.Cloud.Monad

data AwsCloud = AwsCloud

instance CloudProvider AwsCloud where
    type ProviderMonad AwsCloud = AwsMonad
    type ProviderContext AwsCloud = AwsContext

    loadContext = AWS.loadContext

    runCloud (Tagged ctx) (CloudMonad (AwsMonad func)) = runReaderT func ctx

    uploadFile f = CloudMonad $ S3.uploadFile f
    downloadFile s h f = CloudMonad $ S3.downloadFile s h f

    uploadFileInfo e i = CloudMonad $ SDB.uploadFileInfo e i
    uploadFileMetadata e i = CloudMonad $ SDB.uploadFileMetadata e i
    uploadDeleteMarker = CloudMonad . SDB.uploadDeleteMarker
    getAllServerFiles = CloudMonad SDB.getAllServerFiles
    getRecentServerFiles = CloudMonad SDB.getRecentServerFiles

    cleanupCloud = CloudMonad $ deleteOldVersions >> deleteOldDbRecords

runAwsPrivateCloud :: ByteArray ba => FilePath -> [Pattern] -> (T.Text -> IO (Maybe ba)) -> PrivateCloud AwsCloud a -> IO a
runAwsPrivateCloud = runPrivateCloud

runAwsCloud :: Tagged AwsCloud AwsContext -> CloudMonad AwsCloud a -> IO a
runAwsCloud = runCloud

setupAwsPrivateCloud :: ByteArray ba => FilePath -> T.Text -> T.Text -> BS.ByteString -> BS.ByteString -> IO (T.Text, ba)
setupAwsPrivateCloud root cloudid userid accesskeyid secretkey = do
    manager <- newManager tlsManagerSettings
    auth <- makeCredentials accesskeyid secretkey
    let ctx = Tagged AwsContext
            { acBucket = "privatecloud-" <> cloudid
            , acDomain = "privatecloud-" <> cloudid
            , acConf = Configuration
                { timeInfo = Timestamp
                , credentials = auth
                , logger = awsLogger
                , Aws.Aws.proxy = Nothing
                }
            , acManager = manager
            }
    let uniqueId = cloudid <> "--" <> userid

    initCloudSettings root uniqueId
    credential <- runAwsCloud ctx (CloudMonad $ createCloudInstance cloudid userid)
    pure (uniqueId, credential)

connectAwsPrivateCloud :: ByteArray ba => FilePath -> T.Text -> T.Text -> BS.ByteString -> BS.ByteString -> IO (T.Text, ba)
connectAwsPrivateCloud root cloudid userid accesskeyid secretkey = do
    manager <- newManager tlsManagerSettings
    auth <- makeCredentials accesskeyid secretkey
    let ctx = Tagged AwsContext
            { acBucket = "privatecloud-" <> cloudid
            , acDomain = "privatecloud-" <> cloudid
            , acConf = Configuration
                { timeInfo = Timestamp
                , credentials = auth
                , logger = awsLogger
                , Aws.Aws.proxy = Nothing
                }
            , acManager = manager
            }
    let uniqueId = cloudid <> "--" <> userid

    initCloudSettings root uniqueId
    credential <- runAwsCloud ctx (CloudMonad $ connectCloudInstance cloudid userid)
    pure (uniqueId, credential)
