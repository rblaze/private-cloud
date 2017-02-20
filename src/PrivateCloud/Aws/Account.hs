{-# Language OverloadedStrings #-}
module PrivateCloud.Aws.Account where

import Control.Exception.Safe
import Control.Lens
import Control.Monad
import Data.ByteArray as BA
import Data.Monoid
import Data.Tagged
import Network.AWS
import Network.AWS.IAM as IAM hiding (AccessKey)
import Network.AWS.S3 as S3
--import Network.AWS.SDB as SDB
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import PrivateCloud.Aws.Monad
import PrivateCloud.Cloud.Exception

-- XXX temporary imports to workaround amazonka bugs
import Aws.Aws
import Aws.Core
import Aws.SimpleDb as AwsSDB

createCloudInstance :: ByteArray ba => T.Text -> AwsMonad ba
createCloudInstance cloudid = do
    let userName = "privatecloud-user-" <> cloudid
    let policyName = "PrivateCloud-Access-" <> cloudid
    bucketName <- awsAsks acBucket
    domainName <- awsAsks acDomain

    -- create user
    userresp <- send $ IAM.createUser userName
    userarn <- case userresp ^. cursUser of
        Just info -> return $ info ^. uARN
        Nothing -> throw $ CloudInternalError "Invalid CreateUser response"
    accountid <- case T.split (== ':') userarn of
        [_ , _, _, _, accountid, _] -> return accountid
        _ -> throw $ CloudInternalError "Invalid user ARN"

    let BucketName bucketstr = bucketName
    let userPolicy = T.replace "BUCKET" bucketstr $
            T.replace "ACCOUNTID" accountid $
            T.replace "DOMAIN" domainName policyTemplate

    -- set user policy
    void $ send $ IAM.putUserPolicy userName policyName userPolicy
    keyresp <- send $ IAM.createAccessKey & cakUserName ?~ userName

    let info = keyresp ^. cakrsAccessKey
    let keyId = info ^. akAccessKeyId
    let secretKey = info ^. akSecretAccessKey

    -- create storage bucket
    void $ send $ S3.createBucket bucketName
    void $ send $ S3.putBucketVersioning bucketName $
        versioningConfiguration & vcStatus ?~ BVSEnabled
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
    -- drop CreateDomain permission too
    awsconfig <- awsAsks acLegacyConf
    void $ simpleAws awsconfig defServiceConfig $ AwsSDB.createDomain domainName

    -- pack credentials
    return $ BA.concat
        [ T.encodeUtf8 keyId
        , BS.singleton 0
        , T.encodeUtf8 secretKey
        , BS.singleton 0
        , T.encodeUtf8 bucketstr
        , BS.singleton 0
        , T.encodeUtf8 domainName
        ]


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
        { acEnv = env
        , acBucket = BucketName $ T.decodeUtf8 bucketName
        , acDomain = T.decodeUtf8 domainName
        , acLegacyConf = Configuration
            { timeInfo = Timestamp
            , credentials = legacyAuth
            , logger = defaultLog Warning
            }
        }
