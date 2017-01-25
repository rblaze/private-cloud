{-# Language RecordWildCards #-}
module PrivateCloud.Aws.S3 where

import Aws.Aws
import Aws.Core
import Aws.S3
import Control.Monad
import Control.Monad.Trans.Resource
import Data.Conduit
import Data.Conduit.Binary
import Network.HTTP.Client
import System.Log.Logger

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

import PrivateCloud.Aws

s3LoggerName :: String
s3LoggerName = "PrivateCloud.AWS.S3"

uploadFile :: CloudInfo -> FilePath -> BL.ByteString -> IO ()
uploadFile CloudInfo{..} filename body = do
    -- XXX replace with multipart upload, to allow streaming encryption
    let command = putObject ciBucket (T.pack filename) (RequestBodyLBS body)
    noticeM s3LoggerName $ "#S3UPLOAD #file " ++ filename
    void $ memoryAws ciConfig defServiceConfig ciManager command
    infoM s3LoggerName $ "#S3UPLOADED #file " ++ filename

deleteFile :: CloudInfo -> FilePath -> IO ()
deleteFile CloudInfo{..} filename = do
    let command = DeleteObject { doObjectName = T.pack filename, doBucket = ciBucket }
    noticeM s3LoggerName $ "#S3DELETE #file " ++ filename
    void $ memoryAws ciConfig defServiceConfig ciManager command

downloadFile :: CloudInfo -> FilePath -> IO BL.ByteString
downloadFile CloudInfo{..} filename = do
    let command = getObject ciBucket (T.pack filename)
    noticeM s3LoggerName $ "#S3DOWNLOAD #file " ++ filename
    body <- runResourceT $ do
        GetObjectResponse { gorResponse = resp } 
            <- pureAws ciConfig defServiceConfig ciManager command
        -- XXX replace with write to temp file, then move to original.
        -- XXX this will prevent partial downloads and memory overuse.
        responseBody resp $$+- sinkLbs
    infoM s3LoggerName $ "#S3DOWNLOADED #file " ++ filename
    return body
