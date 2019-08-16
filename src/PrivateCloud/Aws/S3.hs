{-# LANGUAGE RecordWildCards #-}
module PrivateCloud.Aws.S3
    ( downloadFile
    , uploadFile
    ) where

import Aws.Core (defServiceConfig)
import Aws.S3 (getObject, multipartUploadSink, GetObjectResponse(..))
import Conduit
import Control.Monad (unless)
import Network.HTTP.Client (responseBody)

import PrivateCloud.Aws.Logging
import PrivateCloud.Aws.Monad
import PrivateCloud.Aws.Util
import PrivateCloud.Cloud.Exception
import PrivateCloud.Provider.Types
import Sodium.Hash

uploadFile :: FilePath -> AwsMonad (StorageId, Length, Hash)
uploadFile localPath = do
    storageId <- StorageId <$> mkUUID
    s3LogInfo $ "S3_UPLOAD_START #objid " ++ show storageId
    -- S3 requires Content-Length header for uploads, and to provide 
    -- it for encrypted data it is necessary to encrypt and keep in memory
    -- all the [multigigabyte] encrypted stream. Another option is to
    -- calculate the size using knowledge of encryption mode, but this
    -- looks like a hack.
    -- Also (TODO) chunked uploads allow for restarts in case of network failures.
    AwsContext{..} <- awsContext
    (len, hash) <- liftIO $ runResourceT $ runConduit $
        sourceFileBS localPath
        .| getZipSink
            ( (,)
            <$> ZipSink lengthCE
            <*> ZipSink hashSink
            <* ZipSink (uploadSink acConf acManager acBucket storageId)
            )

    s3LogInfo $ "S3_UPLOAD_END #objid " ++ show storageId
        ++ " #len " ++ show len ++ " #hash " ++  show hash
    pure (storageId, len, hash)
  where
    uploadChunk = 100*1024*1024
    uploadSink conf manager bucket (StorageId objectId) =
        multipartUploadSink conf defServiceConfig manager bucket objectId uploadChunk
    hashSink = do
        ctx <- liftIO hashInit
        mapM_C (liftIO . hashUpdate ctx)
        encodeHash <$> liftIO (hashFinal ctx)

downloadFile :: StorageId -> Hash -> FilePath -> AwsMonad ()
downloadFile storageId expectedHash localPath = do
    s3LogInfo $ "S3_DOWNLOAD_START #file " ++ show storageId
    AwsContext{..} <- awsContext
    GetObjectResponse{..} <- awsReq $ getObject acBucket (storageid2text storageId)

    liftIO $ runResourceT $ runConduit $
        responseBody gorResponse
        .| getZipSink 
            ( ZipSink hashCheckSink 
            *> ZipSink (sinkFileCautious localPath)
            )

    s3LogInfo $ "S3_DOWNLOAD_END #objid " ++ show storageId
  where
    hashCheckSink = do
        ctx <- liftIO hashInit
        mapM_C (liftIO . hashUpdate ctx)
        realHash <- encodeHash <$> liftIO (hashFinal ctx)
        unless (realHash == expectedHash) $
            throwM $ ServiceInternalError "hmac mismatch in downloaded file"
