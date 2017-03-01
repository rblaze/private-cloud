{-# Language TypeFamilies, OverloadedStrings #-}
module PrivateCloud.Aws.Provider where

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource
import Data.ByteArray
import Data.Monoid
import Data.Tagged
import Network.AWS
import Network.AWS.S3.Types
import System.FilePath.Glob
import qualified Data.Text as T
import qualified Data.ByteString as BS

import PrivateCloud.Aws.Account as AWS
import PrivateCloud.Aws.S3 as S3
import PrivateCloud.Aws.SimpleDb as SDB
import PrivateCloud.Aws.Cleanup
import PrivateCloud.Aws.Monad
import PrivateCloud.Provider.Class
import PrivateCloud.Cloud.Monad

--- FIXME remove when amazonka fixed
import Aws.Aws
import Aws.Core

data AwsCloud = AwsCloud

instance CloudProvider AwsCloud where
    type ProviderMonad AwsCloud = AwsMonad
    type ProviderContext AwsCloud = AwsContext

    newContext = AWS.newContext

    runCloud (Tagged ctx) (CloudMonad (AwsMonad func)) = liftIO $ runResourceT $ runAWS (acEnv ctx) $ runReaderT func ctx

    uploadFile e f = CloudMonad $ S3.uploadFile e f
    downloadFile e v h f = CloudMonad $ S3.downloadFile e v h f

    uploadFileInfo e i = CloudMonad $ SDB.uploadFileInfo e i
    uploadFileMetadata e i = CloudMonad $ SDB.uploadFileMetadata e i
    uploadDeleteMarker = CloudMonad . SDB.uploadDeleteMarker
    getAllServerFiles = CloudMonad SDB.getAllServerFiles
    getRecentServerFiles = CloudMonad SDB.getRecentServerFiles

    cleanupCloud = CloudMonad $ deleteOldVersions >> deleteOldDbRecords

runAwsPrivateCloud :: ByteArray ba => FilePath -> [Pattern] -> (T.Text -> IO (Maybe ba)) -> PrivateCloud AwsCloud a -> IO a
runAwsPrivateCloud = runPrivateCloud

runAwsCloud :: MonadIO m => Tagged AwsCloud AwsContext -> CloudMonad AwsCloud a -> m a
runAwsCloud = runCloud

setupAwsPrivateCloud :: ByteArray ba => FilePath -> T.Text -> T.Text -> BS.ByteString -> BS.ByteString -> IO (T.Text, ba)
setupAwsPrivateCloud root cloudid userid accesskeyid secretkey = do
    env <- newEnv $ FromKeys (AccessKey accesskeyid) (SecretKey secretkey)
    legacyAuth <- makeCredentials accesskeyid secretkey
    let ctx = Tagged AwsContext
            { acEnv = env
            , acBucket = BucketName $ "privatecloud-" <> cloudid
            , acDomain = "privatecloud-" <> cloudid
            , acLegacyConf = Configuration
                { timeInfo = Timestamp
                , credentials = legacyAuth
                , logger = defaultLog Warning
                }
            }
    let uniqueId = cloudid <> "-" <> userid

    initCloudSettings root uniqueId
    credential <- runAwsCloud ctx (CloudMonad $ createCloudInstance cloudid userid)
    return (uniqueId, credential)

connectAwsPrivateCloud :: ByteArray ba => FilePath -> T.Text -> T.Text -> BS.ByteString -> BS.ByteString -> IO (T.Text, ba)
connectAwsPrivateCloud root cloudid userid accesskeyid secretkey = do
    env <- newEnv $ FromKeys (AccessKey accesskeyid) (SecretKey secretkey)
    let ctx = Tagged AwsContext
            { acEnv = env
            , acBucket = BucketName $ "privatecloud-" <> cloudid
            , acDomain = "privatecloud-" <> cloudid
            , acLegacyConf = error "legacy api call unexpected"
            }
    let uniqueId = cloudid <> "-" <> userid

    initCloudSettings root uniqueId
    credential <- runAwsCloud ctx (CloudMonad $ connectCloudInstance cloudid userid)
    return (uniqueId, credential)
