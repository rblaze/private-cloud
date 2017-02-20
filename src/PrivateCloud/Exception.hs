module PrivateCloud.Exception where

import Control.Exception.Safe

data ConfigurationError = ConfigurationError String
    deriving Show

instance Exception ConfigurationError

data CloudInternalError = CloudInternalError String
    deriving Show

instance Exception CloudInternalError

