module PrivateCloud.FileInfo where

import Data.Word
import Data.Time.Clock.POSIX
import qualified Data.ByteString as BS

import PrivateCloud.Aws

dbName :: FilePath
dbName = ".privatecloud"

data LocalFileInfo = LocalFileInfo
    { lfLength :: Word64
    , lfModTime :: POSIXTime
    }
    deriving (Eq, Show)

data DbFileInfo = DbFileInfo
    { dfHash :: BS.ByteString
    , dfLength :: Word64
    , dfModTime :: POSIXTime
    }
    deriving (Eq, Show)

data CloudFileInfo = CloudFileInfo
    { cfHash :: BS.ByteString
    , cfLength :: Word64
    , cfModTime :: POSIXTime
    , cfVersion :: VersionId
    }
    deriving (Eq, Show)

data CloudFileStatus
    = CloudFile CloudFileInfo
    | CloudDeleteMarker
    deriving Show
