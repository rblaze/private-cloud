module PrivateCloud.Cloud.Exception where

import Control.Exception.Safe

newtype ConfigurationError = ConfigurationError String
    deriving Show

instance Exception ConfigurationError

newtype CloudInternalError = CloudInternalError String
    deriving Show

instance Exception CloudInternalError

newtype ServiceInternalError = ServiceInternalError String
    deriving Show

instance Exception ServiceInternalError
