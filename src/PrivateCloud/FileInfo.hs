{-# Language OverloadedStrings, GeneralizedNewtypeDeriving #-}
module PrivateCloud.FileInfo where

import Data.Hashable
import Data.Int
import Data.Text.Buildable
import Data.Word
import Foreign.C.Types
import System.FilePath
import System.Posix.Types
import qualified Data.ByteString as BS
import qualified Data.Text as T

dbName :: FilePath
dbName = ".privatecloud"

-- | Type for filenames used in database.
-- Path separator is always /, no matter what host os we are on.
newtype EntryName = EntryName T.Text
    deriving (Eq, Ord, Hashable)

instance Show EntryName where
    show (EntryName e) = show e

path2entry :: FilePath -> EntryName
path2entry = EntryName . T.intercalate "/" . map T.pack . splitDirectories

entry2path :: EntryName -> FilePath
entry2path (EntryName entry) = joinPath $ map T.unpack $ T.splitOn "/" entry

(<//>) :: EntryName -> EntryName -> EntryName
(EntryName "") <//> next = next
(EntryName base) <//> (EntryName next) = EntryName $ T.concat [base, "/", next]

-- | Type for file timestamp
newtype Timestamp = Timestamp Int64
    deriving (Eq, Ord, Buildable)

instance Show Timestamp where
    show (Timestamp ts) = show ts

epoch2ts :: EpochTime -> Timestamp
epoch2ts (CTime ts) = Timestamp ts

ts2epoch :: Timestamp -> EpochTime 
ts2epoch (Timestamp ts) = fromIntegral ts

-- | Type for S3 version id
newtype VersionId = VersionId T.Text
    deriving Eq

instance Show VersionId where
    show (VersionId v) = show v

versionToText :: VersionId -> T.Text
versionToText (VersionId txt) = txt

data LocalFileInfo = LocalFileInfo
    { lfLength :: Word64
    , lfModTime :: Timestamp
    }
    deriving (Eq, Show)

type LocalFileList = [(EntryName, LocalFileInfo)]

data DbFileInfo = DbFileInfo
    { dfHash :: BS.ByteString
    , dfLength :: Word64
    , dfModTime :: Timestamp
    }
    deriving (Eq, Show)

type DbFileList = [(EntryName, DbFileInfo)]

data CloudFileInfo = CloudFileInfo
    { cfHash :: BS.ByteString
    , cfLength :: Word64
    , cfModTime :: Timestamp
    , cfVersion :: VersionId
    }
    deriving (Eq, Show)

data CloudFileStatus
    = CloudFile CloudFileInfo
    | CloudDeleteMarker
    deriving Show

type CloudFileList = [(EntryName, CloudFileStatus)]
