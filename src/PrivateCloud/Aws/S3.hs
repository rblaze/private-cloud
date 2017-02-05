{-# Language RecordWildCards #-}
module PrivateCloud.Aws.S3 where

import Aws.Aws
import Aws.Core
import Aws.S3
import Conduit
import Control.Exception.Safe
import Control.Monad
import Network.HTTP.Client
import System.Log.Logger

import qualified Data.ByteString.Lazy as BL

import PrivateCloud.Aws
import PrivateCloud.FileInfo

s3LoggerName :: String
s3LoggerName = "PrivateCloud.AWS.S3"

s3LogInfo :: MonadIO m => String -> m ()
s3LogInfo = liftIO . infoM s3LoggerName

s3LogNotice :: MonadIO m => String -> m ()
s3LogNotice = liftIO . noticeM s3LoggerName

s3LogCritical :: MonadIO m => String -> m ()
s3LogCritical = liftIO . criticalM s3LoggerName

uploadFile :: CloudInfo -> EntryName -> FilePath -> IO VersionId
uploadFile CloudInfo{..} (EntryName filename) localPath = do
    s3LogNotice $ "#S3UPLOAD_START #file " ++ show filename
    -- S3 requires Content-Length header for uploads, and to provide 
    -- it for encrypted data it is necessary to encrypt and keep in memory
    -- all the [multigigabyte] encrypted stream. Another option is to
    -- calculate the size using knowledge of encryption mode, but this
    -- looks like hack.
    -- Also (TODO) chunked uploads allow for restarts in case of network failures.
    resp <- runConduitRes $
        sourceFileBS localPath
        .| uploadSink
    s3LogInfo $ "#S3UPLOAD_END #file " ++ show filename
    case cmurVersionId resp of
        Nothing -> throw $ AwsException "no version returned"
        Just version -> return $ VersionId version
    where
    chunkSize = 6 * 1024 * 1024
    uploadSink = do
        uploadId <- liftIO $ imurUploadId <$> memoryAws ciConfig defServiceConfig ciManager (postInitiateMultipartUpload ciBucket filename)
        etags <- chunkedConduit chunkSize
            =$= mapMC (\chunk -> s3LogInfo "#S3UPLOAD_CHUNK" >> return chunk)
            =$= putConduit ciConfig defServiceConfig ciManager ciBucket filename uploadId
            =$= sinkList
        s3LogInfo $ "#S3UPLOAD_COMBINE #file " ++ show filename
        liftIO $ sendEtag ciConfig defServiceConfig ciManager ciBucket filename uploadId etags

deleteFile :: CloudInfo -> EntryName -> IO ()
deleteFile CloudInfo{..} (EntryName filename) = do
    let command = DeleteObject { doObjectName = filename, doBucket = ciBucket }
    s3LogNotice $ "#S3DELETE #file " ++ show filename
    void $ memoryAws ciConfig defServiceConfig ciManager command

downloadFile :: CloudInfo -> EntryName -> VersionId -> IO BL.ByteString
downloadFile CloudInfo{..} (EntryName filename) (VersionId version) = do
    let command = (getObject ciBucket filename)
                    { goVersionId = Just version }
    s3LogNotice $ "#S3DOWNLOAD #file " ++ show filename
    body <- runResourceT $ do
        GetObjectResponse { gorResponse = resp } 
            <- pureAws ciConfig defServiceConfig ciManager command
        -- XXX replace with write to temp file, then move to original.
        -- XXX this will prevent partial downloads and memory overuse.
        responseBody resp $$+- sinkLazy
    s3LogInfo $ "#S3DOWNLOADED #file " ++ show filename
    return body
