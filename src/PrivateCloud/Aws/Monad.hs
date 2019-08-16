{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module PrivateCloud.Aws.Monad where

import Aws.Aws
import Aws.Core
import Aws.S3.Core
import Control.Exception.Safe
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource
import Network.HTTP.Client
import qualified Data.Text as T

data AwsContext = AwsContext
    { acConf :: Configuration
    , acBucket :: Bucket
    , acDomain :: T.Text
    , acManager :: Manager
    }

newtype AwsMonad a = AwsMonad (ReaderT AwsContext IO a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadThrow)

awsContext :: AwsMonad AwsContext
awsContext = AwsMonad ask

awsReq
    :: (Transaction r a,
        DefaultServiceConfiguration (ServiceConfiguration r NormalQuery))
    => r -> AwsMonad a
awsReq req = do
    AwsContext{..} <- awsContext
    liftIO $ runResourceT $ pureAws acConf defServiceConfig acManager req
