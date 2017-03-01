module PrivateCloud.Cloud.Exception where

import Control.Exception.Safe

data ConfigurationError = ConfigurationError String
    deriving Show

instance Exception ConfigurationError

data CloudInternalError = CloudInternalError String
    deriving Show

instance Exception CloudInternalError

data ServiceInternalError = ServiceInternalError String
    deriving Show

instance Exception ServiceInternalError
