{-# Language LambdaCase, FlexibleContexts #-}
module PrivateCloud.Aws.S3 where

import Conduit
import Control.Exception.Safe
import Control.Lens
import Control.Monad
import Control.Monad.Trans.Resource
import Crypto.MAC.HMAC.Conduit
import Crypto.Random
import Data.ByteArray.Encoding
import Data.List.NonEmpty (nonEmpty)
import Network.AWS hiding (await)
import Network.AWS.Data.Body
import Network.AWS.S3
import System.FilePath
import System.Directory
import System.IO
import System.Log.Logger

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as T

import PrivateCloud.Aws.Monad
import PrivateCloud.Cloud.Exception
import PrivateCloud.Provider.Types

s3LoggerName :: String
s3LoggerName = "PrivateCloud.AWS.S3"

s3LogInfo :: MonadIO m => String -> m ()
s3LogInfo = liftIO . infoM s3LoggerName

s3LogNotice :: MonadIO m => String -> m ()
s3LogNotice = liftIO . noticeM s3LoggerName

s3LogCritical :: MonadIO m => String -> m ()
s3LogCritical = liftIO . criticalM s3LoggerName

hmacKey :: BS.ByteString
hmacKey = BS.pack $ map (fromIntegral . fromEnum) "No, this constant key will NOT be left in release, thank you"

createStorageId :: MonadIO m => m StorageId
createStorageId = do
    rnd <- liftIO getSystemDRG
    return $ StorageId $ T.decodeUtf8 $ fst $ withRandomBytes rnd 16 $ \m ->
        convertToBase Base64URLUnpadded (m :: BS.ByteString)

uploadFile :: FilePath -> AwsMonad (StorageId, Length, Hash)
uploadFile localPath = do
    storageId <- createStorageId
    s3LogNotice $ "#S3UPLOAD_START #objid " ++ show storageId
    -- S3 requires Content-Length header for uploads, and to provide 
    -- it for encrypted data it is necessary to encrypt and keep in memory
    -- all the [multigigabyte] encrypted stream. Another option is to
    -- calculate the size using knowledge of encryption mode, but this
    -- looks like hack.
    -- Also (TODO) chunked uploads allow for restarts in case of network failures.
    bucketName <- awsAsks acBucket
    (len, hmac) <- runConduit $
        sourceFileBS localPath
        .| getZipSink (ZipSink (uploadSink bucketName storageId)
                *> ((,) <$> ZipSink lengthCE <*> ZipSink (sinkHMAC hmacKey)))
    let hash = hmac2hash hmac
    s3LogInfo $ "#S3UPLOAD_END #objid " ++ show storageId
        ++ " #len " ++ show len ++ " #hash " ++  show hash
    return (storageId, len, hash)
  where
    chunkSize = 6 * 1024 * 1024
    uploadSink bucketName storageId = do
        let object = ObjectKey (storage2text storageId)
        resp <- lift $ send $ createMultipartUpload bucketName object
        uploadId <- mustHave "no uploadid returned" $ resp ^. cmursUploadId
        etags <- chunkedConduit chunkSize
            =$= mapMC (\chunk -> s3LogInfo "#S3UPLOAD_CHUNK" >> return chunk)
            =$= putConduit bucketName object uploadId
            =$= sinkList
        sentParts <- mustHave "empty parts list" $ nonEmpty $ zipWith completedPart [1 ..] etags
        s3LogInfo $ "#S3UPLOAD_COMBINE #objid " ++ show storageId
        lift $ send $ completeMultipartUpload bucketName object uploadId
            & cMultipartUpload ?~ (completedMultipartUpload & cmuParts ?~ sentParts)
                    
    putConduit bucketName object uploadId = loop 1
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

downloadFile :: StorageId -> Hash -> FilePath -> AwsMonad ()
downloadFile storageId expectedHash localPath = do
    s3LogNotice $ "#S3DOWNLOAD #file " ++ show storageId
    bucketName <- awsAsks acBucket
    resp <- send $ getObject bucketName (ObjectKey $ storage2text storageId)
    liftResourceT $ resourceMask $ \unmask -> do
        let dir = takeDirectory localPath
        let file = takeFileName localPath
        (name, h) <- liftIO $ openBinaryTempFile dir file
        handleReleaseKey <- register $ hClose h
        fileReleaseKey <- register $ removeFile name
        unmask $ do
            let RsBody stream = resp ^. gorsBody
            let sink = getZipSink (ZipSink (sinkHandle h) *> ZipSink (sinkHMAC hmacKey))
            hmac <- stream $$+- sink
            let hash = hmac2hash hmac
            when (hash /= expectedHash) $
                throw $ ServiceInternalError "hmac mismatch in downloaded file"
            release handleReleaseKey
        liftIO $ renameFile name localPath
        void $ unprotect fileReleaseKey -- no longer needed to delete file
    s3LogInfo $ "#S3DOWNLOADED #objid " ++ show storageId

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
