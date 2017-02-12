{-# Language RecordWildCards #-}
module PrivateCloud.Aws.S3 where

import Aws.Aws
import Aws.Core
import Aws.S3
import Conduit
import Control.Exception.Safe
import Control.Monad
import Crypto.MAC.HMAC.Conduit
import Data.Word
import Network.HTTP.Client
import System.Log.Logger

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import PrivateCloud.Aws
import PrivateCloud.FileInfo
import PrivateCloud.ServiceConfig

s3LoggerName :: String
s3LoggerName = "PrivateCloud.AWS.S3"

s3LogInfo :: MonadIO m => String -> m ()
s3LogInfo = liftIO . infoM s3LoggerName

s3LogNotice :: MonadIO m => String -> m ()
s3LogNotice = liftIO . noticeM s3LoggerName

s3LogCritical :: MonadIO m => String -> m ()
s3LogCritical = liftIO . criticalM s3LoggerName

hmacKey :: BS.ByteString
hmacKey = BS.pack [102,111,111,98,97,114]

uploadFile :: ServiceConfig -> EntryName -> FilePath -> IO (VersionId, Word64, Hash)
uploadFile ServiceConfig{..} (EntryName filename) localPath = do
    s3LogNotice $ "#S3UPLOAD_START #file " ++ show filename
    -- S3 requires Content-Length header for uploads, and to provide 
    -- it for encrypted data it is necessary to encrypt and keep in memory
    -- all the [multigigabyte] encrypted stream. Another option is to
    -- calculate the size using knowledge of encryption mode, but this
    -- looks like hack.
    -- Also (TODO) chunked uploads allow for restarts in case of network failures.
    (resp, len, hmac) <- runConduitRes $
        sourceFileBS localPath
        .| getZipSink ((,,) <$> ZipSink uploadSink
                <*> ZipSink lengthCE
                <*> ZipSink (sinkHMAC hmacKey))
    let hash = hmac2hash hmac
    s3LogInfo $ "#S3UPLOAD_END #file " ++ show filename
        ++ " #len " ++ show len ++ " #hash " ++  show hash
    case cmurVersionId resp of
        Nothing -> throw $ AwsException "no version returned"
        Just version -> return (VersionId version, len, hash)
    where
    chunkSize = 6 * 1024 * 1024
    uploadSink = do
        uploadId <- liftIO $ imurUploadId <$> memoryAws scConfig defServiceConfig scManager (postInitiateMultipartUpload scBucket filename)
        etags <- chunkedConduit chunkSize
            =$= mapMC (\chunk -> s3LogInfo "#S3UPLOAD_CHUNK" >> return chunk)
            =$= putConduit scConfig defServiceConfig scManager scBucket filename uploadId
            =$= sinkList
        s3LogInfo $ "#S3UPLOAD_COMBINE #file " ++ show filename
        liftIO $ sendEtag scConfig defServiceConfig scManager scBucket filename uploadId etags

deleteFile :: ServiceConfig -> EntryName -> IO ()
deleteFile ServiceConfig{..} (EntryName filename) = do
    let command = DeleteObject { doObjectName = filename, doBucket = scBucket }
    s3LogNotice $ "#S3DELETE #file " ++ show filename
    void $ memoryAws scConfig defServiceConfig scManager command

downloadFile :: ServiceConfig -> EntryName -> VersionId -> IO BL.ByteString
downloadFile ServiceConfig{..} (EntryName filename) (VersionId version) = do
    let command = (getObject scBucket filename)
                    { goVersionId = Just version }
    s3LogNotice $ "#S3DOWNLOAD #file " ++ show filename
    body <- runResourceT $ do
        GetObjectResponse { gorResponse = resp } 
            <- pureAws scConfig defServiceConfig scManager command
        -- XXX replace with write to temp file, then move to original.
        -- this will prevent partial downloads and memory overuse.
        responseBody resp $$+- sinkLazy
    s3LogInfo $ "#S3DOWNLOADED #file " ++ show filename
    return body
