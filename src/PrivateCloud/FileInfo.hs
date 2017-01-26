module PrivateCloud.FileInfo where

import Data.Word
import System.Posix.Types
import qualified Data.ByteString as BS

import PrivateCloud.Aws

dbName :: FilePath
dbName = ".privatecloud"

data LocalFileInfo = LocalFileInfo
    { lfLength :: Word64
    , lfModTime :: EpochTime
    }
    deriving (Eq, Show)

data DbFileInfo = DbFileInfo
    { dfHash :: BS.ByteString
    , dfLength :: Word64
    , dfModTime :: EpochTime
    }
    deriving (Eq, Show)

data CloudFileInfo = CloudFileInfo
    { cfHash :: BS.ByteString
    , cfLength :: Word64
    , cfModTime :: EpochTime
    , cfVersion :: ObjectVersion
    }
    deriving (Eq, Show)
