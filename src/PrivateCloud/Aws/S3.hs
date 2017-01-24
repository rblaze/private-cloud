{-# Language RecordWildCards #-}
module PrivateCloud.Aws.S3 where

import Aws.Aws
import Aws.Core
import Aws.S3
import Control.Monad
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
    infoM s3LoggerName $ "#S3UPLOAD #file " ++ filename
    void $ memoryAws ciConfig defServiceConfig ciManager command
    infoM s3LoggerName $ "#S3UPLOADED #file " ++ filename

deleteFile :: CloudInfo -> FilePath -> IO ()
deleteFile CloudInfo{..} filename = do
    let command = DeleteObject { doObjectName = T.pack filename, doBucket = ciBucket }
    infoM s3LoggerName $ "#S3DELETE #file " ++ filename
    void $ memoryAws ciConfig defServiceConfig ciManager command
