{-# Language GeneralizedNewtypeDeriving #-}
module PrivateCloud.Aws.Monad where

import Control.Exception.Safe
import Control.Monad.Base
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource
import Network.AWS
import Network.AWS.S3
import qualified Data.Text as T

import Aws.Aws

data AwsContext = AwsContext
    { acEnv :: Env
    , acBucket :: BucketName
    , acDomain :: T.Text
    , acLegacyConf :: Configuration
    }

-- technically, Env is not needed inside monad, but I'm too lazy to write another type
newtype AwsMonad a = AwsMonad (ReaderT AwsContext AWS a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadCatch, MonadThrow, MonadAWS, MonadBase IO, MonadResource)

awsAsks :: (AwsContext -> a) -> AwsMonad a
awsAsks = AwsMonad . asks
