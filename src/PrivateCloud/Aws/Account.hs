{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module PrivateCloud.Aws.Account
    ( connectCloudInstance
    , createCloudInstance
    , loadContext
    ) where

import Aws.Aws
import Aws.Core
import Aws.Iam
import Aws.S3
import Aws.SimpleDb
import Control.Exception.Safe
import Control.Monad
import Data.ByteArray as BA
import Data.Tagged
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import PrivateCloud.Aws.Monad
import PrivateCloud.Aws.Logging
import PrivateCloud.Cloud.Exception

createCloudInstance :: ByteArray ba => T.Text -> T.Text -> AwsMonad ba
createCloudInstance cloudid userid = do
    let groupName = "pcg-" <> cloudid
    let userName = T.take 64 ("pcu-" <> cloudid <> "_" <> userid)
    let policyName = "PrivateCloud-Access-" <> cloudid

    AwsContext{..} <- awsContext

    -- create storage bucket
    void $ awsReq $ putBucket acBucket

    -- create domain
    void $ awsReq $ createDomain acDomain

    -- create IAM group
    CreateGroupResponse group <- awsReq $ CreateGroup groupName Nothing

    accountid <- case T.split (== ':') (groupArn group) of
        [_ , _, _, _, accountid, _] -> pure accountid
        _ -> throw $ CloudInternalError "Invalid group ARN"

    -- set group policy
    let policyText = T.replace "BUCKET" acBucket $
            T.replace "ACCOUNTID" accountid $
            T.replace "DOMAIN" acDomain policyTemplate

    void $ awsReq $ PutGroupPolicy policyText policyName groupName

    -- create user and return its access key
    createCredentials groupName userName

policyTemplate :: T.Text
policyTemplate = "{                                                 \
\    \"Version\": \"2012-10-17\",                                   \
\    \"Statement\": [                                               \
\        {                                                          \
\            \"Sid\": \"AllowStorageAccess\",                       \
\            \"Effect\": \"Allow\",                                 \
\            \"Action\": [                                          \
\                \"s3:AbortMultipartUpload\",                       \
\                \"s3:DeleteObject\",                               \
\                \"s3:DeleteObjectVersion\",                        \
\                \"s3:GetObject\",                                  \
\                \"s3:GetObjectVersion\",                           \
\                \"s3:ListBucket\",                                 \
\                \"s3:ListBucketMultipartUploads\",                 \
\                \"s3:ListBucketVersions\",                         \
\                \"s3:ListMultipartUploadParts\",                   \
\                \"s3:PutObject\"                                   \
\            ],                                                     \
\            \"Resource\": [                                        \
\                \"arn:aws:s3:::BUCKET/*\",                         \
\                \"arn:aws:s3:::BUCKET\"                            \
\            ]                                                      \
\        },                                                         \
\        {                                                          \
\            \"Sid\": \"AllowDbAccess\",                            \
\            \"Effect\": \"Allow\",                                 \
\            \"Action\": [                                          \
\                \"sdb:BatchDeleteAttributes\",                     \
\                \"sdb:BatchPutAttributes\",                        \
\                \"sdb:DeleteAttributes\",                          \
\                \"sdb:GetAttributes\",                             \
\                \"sdb:PutAttributes\",                             \
\                \"sdb:Select\"                                     \
\            ],                                                     \
\            \"Resource\": [                                        \
\                \"arn:aws:sdb:*:ACCOUNTID:domain/DOMAIN\"          \
\            ]                                                      \
\        }                                                          \
\    ]                                                              \
\}"

loadContext :: ByteArray ba => Tagged p ba -> IO AwsContext
loadContext (Tagged creds) = do
    [keyid, key, bucketName, domainName] <-
        case BS.split 0 (convert creds) of
            v@[_, _, _, _] -> pure v
            _ -> throw $ CloudInternalError "Invalid credentials format"
    auth <- makeCredentials keyid key
    manager <- newManager tlsManagerSettings
    pure AwsContext
        { acConf = Configuration
            { timeInfo = Timestamp
            , credentials = auth
            , logger = awsLogger
            , Aws.Aws.proxy = Nothing
            }
        , acBucket = T.decodeUtf8 bucketName
        , acDomain = T.decodeUtf8 domainName
        , acManager = manager
        }

connectCloudInstance :: ByteArray ba => T.Text -> T.Text -> AwsMonad ba
connectCloudInstance cloudid userid = do
    let groupName = "pcg-" <> cloudid
    let userName = T.take 64 ("pcu-" <> cloudid <> "_" <> userid)

    -- create user and return its access key
    createCredentials groupName userName

createCredentials :: ByteArray ba => T.Text -> T.Text -> AwsMonad ba
createCredentials groupName userName = do
    AwsContext{..} <- awsContext

    -- create user
    void $ awsReq $ CreateUser userName Nothing
    -- add user to group
    void $ awsReq $ AddUserToGroup groupName userName
    -- create access key
    CreateAccessKeyResponse AccessKey{..} <- awsReq $ CreateAccessKey (Just userName)

    -- pack credentials
    pure $ BA.concat
        [ T.encodeUtf8 akAccessKeyId
        , BS.singleton 0
        , T.encodeUtf8 akSecretAccessKey
        , BS.singleton 0
        , T.encodeUtf8 acBucket
        , BS.singleton 0
        , T.encodeUtf8 acDomain
        ]
