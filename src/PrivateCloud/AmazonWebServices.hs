{-# Language TypeFamilies, OverloadedStrings #-}
module PrivateCloud.AmazonWebServices where

import Control.Monad.Trans.Resource
import Network.AWS

import PrivateCloud.CloudProvider
import qualified PrivateCloud.Aws.Account as AWS

data AwsCloud = AwsCloud

instance CloudProvider AwsCloud where
    type ProviderMonad AwsCloud = AWS
    type ProviderContext AwsCloud = Env

    runCloud env (CloudMonad func) = runAWS env func
    createCloudInstance = AWS.createCloudInstance

runAwsCloud :: MonadResource m => Env -> CloudMonad AwsCloud a -> m a
runAwsCloud = runCloud
