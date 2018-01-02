{-# Language OverloadedStrings #-}
module PrivateCloud.Aws.Account
    ( connectCloudInstance
    , createCloudInstance
    , newContext
    ) where

import Control.Exception.Safe
import Control.Lens
import Control.Monad
import Data.ByteArray as BA
import Data.Monoid
import Data.Tagged
import Network.AWS
import Network.AWS.IAM as IAM
import Network.AWS.S3 as S3
--import Network.AWS.SDB as SDB
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import PrivateCloud.Aws.Monad
import PrivateCloud.Aws.Logging
import PrivateCloud.Cloud.Exception

-- XXX temporary imports to workaround amazonka bugs
import Aws.Aws
import Aws.Core
import Aws.SimpleDb as AwsSDB

createCloudInstance :: ByteArray ba => T.Text -> T.Text -> AwsMonad ba
createCloudInstance cloudid userid = do
    let groupName = "privatecloud-group-" <> cloudid
    let userName = "privatecloud-user-" <> cloudid <> "-" <> userid
    let policyName = "PrivateCloud-Access-" <> cloudid
    bucketName <- awsAsks acBucket
    domainName <- awsAsks acDomain

    -- create storage bucket
    void $ send $ S3.createBucket bucketName
    {- XXX: API broken in amazonka
    void $ send $
        S3.putBucketLifecycleConfiguration bucketName & pblcLifecycleConfiguration ?~
            (bucketLifecycleConfiguration & blcRules .~
                [ lifecycleRule ESEnabled & lrAbortIncompleteMultipartUpload ?~
                    (abortIncompleteMultipartUpload & (aimuDaysAfterInitiation ?~ 2))
                ]
            )
    -}

    -- create domain
    {- XXX: API broken in amazonka
    void $ send $ SDB.createDomain domainName
    -}
    -- XXX create domain via aws call
    awsconfig <- awsAsks acLegacyConf
    void $ simpleAws awsconfig defServiceConfig $ AwsSDB.createDomain domainName

    -- create group
    groupResp <- send $ IAM.createGroup groupName

    let groupArn = groupResp ^. cgrsGroup ^. gARN
    accountid <- case T.split (== ':') groupArn of
        [_ , _, _, _, accountid, _] -> return accountid
        _ -> throw $ CloudInternalError "Invalid group ARN"

    -- set group policy
    let BucketName bucketstr = bucketName
    let policyText = T.replace "BUCKET" bucketstr $
            T.replace "ACCOUNTID" accountid $
            T.replace "DOMAIN" domainName policyTemplate

    void $ send $ IAM.putGroupPolicy groupName policyName policyText

    -- create user and return its access key
    createCredentials groupName userName bucketName domainName

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

newContext :: ByteArray ba => Tagged p ba -> IO AwsContext
newContext (Tagged creds) = do
    [keyid, key, bucketName, domainName] <-
        case BS.split 0 (convert creds) of
            v@[_, _, _, _] -> return v
            _ -> throw $ CloudInternalError "Invalid credentials format"
    env <- newEnv $ FromKeys (AccessKey keyid) (SecretKey key)
    legacyAuth <- makeCredentials keyid key
    return AwsContext
        { acEnv = env & envLogger .~ amazonkaLogger
        , acBucket = BucketName $ T.decodeUtf8 bucketName
        , acDomain = T.decodeUtf8 domainName
        , acLegacyConf = Configuration
            { timeInfo = Timestamp
            , credentials = legacyAuth
            , logger = defaultLog Warning
            , Aws.Aws.proxy = Nothing
            }
        }

connectCloudInstance :: ByteArray ba => T.Text -> T.Text -> AwsMonad ba
connectCloudInstance cloudid userid = do
    let groupName = "privatecloud-group-" <> cloudid
    let userName = "privatecloud-user-" <> cloudid <> "-" <> userid
    bucketName <- awsAsks acBucket
    domainName <- awsAsks acDomain

    -- create user and return its access key
    createCredentials groupName userName bucketName domainName

createCredentials :: ByteArray ba => T.Text -> T.Text -> BucketName -> T.Text -> AwsMonad ba
createCredentials groupName userName (BucketName bucketName) domainName = do
    -- create user
    void $ send $ IAM.createUser userName
    -- add user to group
    void $ send $ IAM.addUserToGroup groupName userName
    -- create access key
    keyresp <- send $ IAM.createAccessKey & cakUserName ?~ userName

    let info = keyresp ^. cakrsAccessKey
    let AccessKey keyId = info ^. akiAccessKeyId
    let secretKey = info ^. akiSecretAccessKey

    -- pack credentials
    return $ BA.concat
        [ keyId
        , BS.singleton 0
        , T.encodeUtf8 secretKey
        , BS.singleton 0
        , T.encodeUtf8 bucketName
        , BS.singleton 0
        , T.encodeUtf8 domainName
        ]
