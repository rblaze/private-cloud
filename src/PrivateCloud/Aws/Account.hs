{-# Language OverloadedStrings #-}
module PrivateCloud.Aws.Account where

import Control.Exception.Safe
import Control.Lens
import Control.Monad
import Data.Monoid
import Network.AWS
import Network.AWS.IAM as IAM hiding (AccessKey)
import Network.AWS.S3 as S3
import qualified Data.Text as T

data InternalError = InternalError String
    deriving (Show)

instance Exception InternalError

bucketName :: T.Text -> T.Text
bucketName = ("privatecloud-" <>)

createUser :: MonadAWS m => T.Text -> m (T.Text, T.Text)
createUser accountId = do
    let username = "privatecloud-user-" <> accountId
    let domain = "privatecloud-" <> accountId
    userresp <- send $ IAM.createUser username
    userarn <- case userresp ^. cursUser of
        Just info -> return $ info ^. uARN
        Nothing -> throw $ InternalError "Invalid CreateUser response"
    accountid <- case T.split (== ':') userarn of
        [_ , _, _, _, accountid, _] -> return accountid
        _ -> throw $ InternalError "Invalid user ARN"

    let userPolicy = T.replace "BUCKET" (bucketName accountId) $
            T.replace "ACCOUNTID" accountid $
            T.replace "DOMAIN" domain policyTemplate
    let policyName = "PrivateCloud-Access-" <> accountId

    void $ send $ IAM.putUserPolicy username policyName userPolicy
    keyresp <- send $ IAM.createAccessKey & cakUserName ?~ username
    let info = keyresp ^. cakrsAccessKey
    return (info ^. akAccessKeyId, info ^. akSecretAccessKey)

createBucket :: MonadAWS m => T.Text -> m ()
createBucket accountId = do
    let bucketname = BucketName $ bucketName accountId
    void $ send $ S3.createBucket bucketname
    void $ send $ S3.putBucketVersioning bucketname $
        versioningConfiguration & vcStatus ?~ BVSEnabled
    {- XXX: API broken in amazonka
    void $ send $
        S3.putBucketLifecycleConfiguration bucketname & pblcLifecycleConfiguration ?~
            (bucketLifecycleConfiguration & blcRules .~
                [ lifecycleRule ESEnabled & lrAbortIncompleteMultipartUpload ?~
                    (abortIncompleteMultipartUpload & (aimuDaysAfterInitiation ?~ 2))
                ]
            )
    -}

policyTemplate :: T.Text
policyTemplate = "{                                                 \
\    \"Version\": \"2012-10-17\",                                   \
\    \"Statement\": [                                               \
\        {                                                          \
\            \"Sid\": \"AllowStorageAccess\",                       \
\            \"Effect\": \"Allow\",                                 \
\            \"Action\": [                                          \
\                \"s3:AbortMultipartUpload\",                       \
\                \"s3:CreateBucket\",                               \
\                \"s3:DeleteBucket\",                               \
\                \"s3:DeleteObject\",                               \
\                \"s3:DeleteObjectVersion\",                        \
\                \"s3:GetObject\",                                  \
\                \"s3:GetObjectVersion\",                           \
\                \"s3:ListBucket\",                                 \
\                \"s3:ListBucketMultipartUploads\",                 \
\                \"s3:ListBucketVersions\",                         \
\                \"s3:ListMultipartUploadParts\",                   \
\                \"s3:PutBucketVersioning\",                        \
\                \"s3:PutLifecycleConfiguration\",                  \
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
\                \"sdb:Select\",                                    \
\                \"sdb:CreateDomain\",                              \
\                \"sdb:DeleteDomain\"                               \
\            ],                                                     \
\            \"Resource\": [                                        \
\                \"arn:aws:sdb:*:ACCOUNTID:domain/DOMAIN\"          \
\            ]                                                      \
\        }                                                          \
\    ]                                                              \
\}"
