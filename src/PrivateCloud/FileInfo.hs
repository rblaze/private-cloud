module PrivateCloud.FileInfo where

import Data.Word
import System.Posix.Types
import qualified Data.ByteString as BS

dbName :: FilePath
dbName = ".privatecloud"

data FileInfo = FileInfo
    { fiHash :: BS.ByteString
    , fiLength :: Word64
    , fiModTime :: EpochTime
    }
    deriving (Eq, Show)
