{-# Language LambdaCase, RecordWildCards, FlexibleContexts #-}
module PrivateCloud.Aws.S3 where

import Conduit
import Control.Exception.Safe
import Control.Lens
import Control.Monad.Trans.Resource
import Crypto.MAC.HMAC.Conduit
import Data.List.NonEmpty (nonEmpty)
import Network.AWS hiding (await)
import Network.AWS.Data.Body
import Network.AWS.S3
import System.Log.Logger

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import PrivateCloud.Aws.Monad
import PrivateCloud.Cloud.Exception
import PrivateCloud.Provider.FileInfo

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

uploadFile :: EntryName -> FilePath -> AwsMonad (VersionId, Length, Hash)
uploadFile (EntryName filename) localPath = do
    s3LogNotice $ "#S3UPLOAD_START #file " ++ show filename
    -- S3 requires Content-Length header for uploads, and to provide 
    -- it for encrypted data it is necessary to encrypt and keep in memory
    -- all the [multigigabyte] encrypted stream. Another option is to
    -- calculate the size using knowledge of encryption mode, but this
    -- looks like hack.
    -- Also (TODO) chunked uploads allow for restarts in case of network failures.
    bucketName <- awsAsks acBucket
    (resp, len, hmac) <- runConduit $
        sourceFileBS localPath
        .| getZipSink ((,,) <$> ZipSink (uploadSink bucketName)
                <*> ZipSink lengthCE
                <*> ZipSink (sinkHMAC hmacKey))
    let hash = hmac2hash hmac
    s3LogInfo $ "#S3UPLOAD_END #file " ++ show filename
        ++ " #len " ++ show len ++ " #hash " ++  show hash
    ObjectVersionId version <- mustHave "no version returned" $ resp ^. crsVersionId
    return (VersionId version, len, hash)
  where
    object = ObjectKey filename
    chunkSize = 6 * 1024 * 1024
    uploadSink bucketName = do
        resp <- lift $ send $ createMultipartUpload bucketName object
        uploadId <- mustHave "no uploadid returned" $ resp ^. cmursUploadId
        etags <- chunkedConduit chunkSize
            =$= mapMC (\chunk -> s3LogInfo "#S3UPLOAD_CHUNK" >> return chunk)
            =$= putConduit bucketName uploadId
            =$= sinkList
        sentParts <- mustHave "empty parts list" $ nonEmpty $ zipWith completedPart [1 ..] etags
        s3LogInfo $ "#S3UPLOAD_COMBINE #file " ++ show filename
        lift $ send $ completeMultipartUpload bucketName object uploadId
            & cMultipartUpload ?~ (completedMultipartUpload & cmuParts ?~ sentParts)
                    
    putConduit bucketName uploadId = loop 1
      where
        loop n = do
            v' <- await
            case v' of
                Just v -> do
                    resp <- lift $ send $ uploadPart bucketName object n uploadId (toBody v)
                    etag <- mustHave "no etag returned" $ resp ^. uprsETag
                    yield etag
                    loop (n+1)
                Nothing -> return ()

{-
deleteFile :: EntryName -> AwsMonad ()
deleteFile (EntryName filename) = do
    s3LogNotice $ "#S3DELETE #file " ++ show filename
    bucketName <- awsAsks acBucket
    void $ send $ deleteObject bucketName (ObjectKey filename)
-}

downloadFile :: EntryName -> VersionId -> FilePath -> AwsMonad ()
downloadFile (EntryName filename) (VersionId version) localPath = do
    s3LogNotice $ "#S3DOWNLOAD #file " ++ show filename
    bucketName <- awsAsks acBucket
    resp <- send $ getObject bucketName (ObjectKey filename)
                    & goVersionId ?~ ObjectVersionId version
    -- XXX replace with write to temp file, then move to original.
    -- this will prevent partial downloads and memory overuse.
    let RsBody stream = resp ^. gorsBody
    liftResourceT (stream $$+- sinkFile localPath)
    s3LogInfo $ "#S3DOWNLOADED #file " ++ show filename

chunkedConduit :: MonadResource m => Integer -> Conduit BS.ByteString m BL.ByteString
chunkedConduit size = loop 0 []
  where
    loop :: Monad m => Integer -> [BS.ByteString] -> Conduit BS.ByteString m BL.ByteString
    loop cnt str = await >>= maybe (yieldChunk str) go
      where
        go :: Monad m => BS.ByteString -> Conduit BS.ByteString m BL.ByteString
        go line
            | size <= len = yieldChunk newStr >> loop 0 []
            | otherwise   = loop len newStr
          where
            len = fromIntegral (BS.length line) + cnt
            newStr = line:str

    yieldChunk :: Monad m => [BS.ByteString] -> Conduit i m BL.ByteString
    yieldChunk = yield . BL.fromChunks . reverse

mustHave :: MonadThrow m => String -> Maybe a -> m a
mustHave str = \case
    Just v -> return v
    Nothing -> throw $ CloudInternalError str
