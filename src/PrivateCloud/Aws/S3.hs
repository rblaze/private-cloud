{-# Language RecordWildCards #-}
module PrivateCloud.Aws.S3 where

import Aws.Aws
import Aws.Core
import Aws.S3
import Control.Exception.Safe
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Conduit
import Data.Conduit.Binary
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

uploadFile :: CloudInfo -> EntryName -> BL.ByteString -> IO VersionId
uploadFile CloudInfo{..} (EntryName filename) body = do
    -- XXX replace with multipart upload, to allow streaming encryption
    let command = putObject ciBucket filename (RequestBodyLBS body)
    s3LogNotice $ "#S3UPLOAD #file " ++ show filename
    resp <- memoryAws ciConfig defServiceConfig ciManager command
    s3LogInfo $ "#S3UPLOADED #file " ++ show filename
    case porVersionId resp of
        Nothing -> throw $ AwsException "no version returned"
        Just version -> return $ VersionId version

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
        responseBody resp $$+- sinkLbs
    s3LogInfo $ "#S3DOWNLOADED #file " ++ show filename
    return body
