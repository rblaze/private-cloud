{-# Language OverloadedStrings, FlexibleContexts #-}
module PrivateCloud.Aws.Account where

import Control.Exception.Safe
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteArray
import Data.ByteArray.Pack
import Data.Monoid
import Network.AWS
import Network.AWS.IAM as IAM hiding (AccessKey)
import Network.AWS.S3 as S3
import Network.AWS.SDB as SDB
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import PrivateCloud.CloudProvider

-- XXX temporary imports to workaround amazonka bugs
import Aws.Aws
import Aws.Core
import Aws.SimpleDb as AwsSDB

data AwsInternalError = AwsInternalError String
    deriving Show

instance Exception AwsInternalError

createCloudInstance :: (MonadAWS (CloudMonad p), ByteArray ba) => T.Text -> CloudMonad p ba
createCloudInstance instanceId = do
    let username = "privatecloud-user-" <> instanceId
    let domainName = "privatecloud-" <> instanceId
    let bucketName = "privatecloud-" <> instanceId
    let policyName = "PrivateCloud-Access-" <> instanceId

    -- create user
    userresp <- send $ IAM.createUser username
    userarn <- case userresp ^. cursUser of
        Just info -> return $ info ^. uARN
        Nothing -> throw $ AwsInternalError "Invalid CreateUser response"
    accountid <- case T.split (== ':') userarn of
        [_ , _, _, _, accountid, _] -> return accountid
        _ -> throw $ AwsInternalError "Invalid user ARN"

    let userPolicy = T.replace "BUCKET" bucketName $
            T.replace "ACCOUNTID" accountid $
            T.replace "DOMAIN" domainName policyTemplate

    -- set user policy
    void $ send $ IAM.putUserPolicy username policyName userPolicy
    keyresp <- send $ IAM.createAccessKey & cakUserName ?~ username

    let info = keyresp ^. cakrsAccessKey
    let keyId = info ^. akAccessKeyId
    let secretKey = info ^. akSecretAccessKey

    -- create storage bucket
    void $ send $ S3.createBucket (BucketName bucketName)
    void $ send $ S3.putBucketVersioning (BucketName bucketName) $
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

    -- pack credentials
    let keyidstr = T.encodeUtf8 keyId
    let secretkeystr = T.encodeUtf8 secretKey
    let totalSize = BS.length keyidstr + 1 + BS.length secretkeystr

    -- XXX create domain via aws call
    cred <- makeCredentials keyidstr secretkeystr
    let awsconfig = Configuration
            { timeInfo = Timestamp
            , credentials = cred
            , logger = defaultLog Warning
            }
    void $ liftIO $ simpleAws awsconfig defServiceConfig $ AwsSDB.createDomain domainName

    case fill totalSize $ putBytes keyidstr >> putWord8 32 >> putBytes secretkeystr of
        Left err -> throw $ AwsInternalError err
        Right ba -> return ba


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
\                \"sdb:CreateDomain\",                              \
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
