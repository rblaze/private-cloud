{-# Language OverloadedStrings, GeneralizedNewtypeDeriving #-}
module PrivateCloud.Provider.FileInfo where

import Crypto.Hash
import Crypto.MAC.HMAC
import Data.ByteArray.Encoding
import Data.Hashable
import Data.Int
import Data.Text.Buildable
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Word
import Foreign.C.Types
import System.FilePath
import System.Posix.Types
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

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

utc2ts :: UTCTime -> Timestamp
utc2ts = Timestamp . round . utcTimeToPOSIXSeconds

ts2utc :: Timestamp -> UTCTime 
ts2utc (Timestamp ts) = posixSecondsToUTCTime $ realToFrac ts

-- | Type for S3 version id
newtype VersionId = VersionId { version2text :: T.Text }
    deriving Eq

instance Show VersionId where
    show (VersionId v) = show v

-- | Type for file hash
newtype Hash = Hash { hash2text :: T.Text }
    deriving Eq

instance Show Hash where
    show (Hash h) = show h

hmac2hash :: HMAC SHA512t_256 -> Hash
hmac2hash = Hash . T.decodeUtf8 . convertToBase Base64

type Length = Word64

data LocalFileInfo = LocalFileInfo
    { lfLength :: Length
    , lfModTime :: Timestamp
    }
    deriving (Eq, Show)

type LocalFileList = [(EntryName, LocalFileInfo)]

data DbFileInfo = DbFileInfo
    { dfHash :: Hash
    , dfLength :: Length
    , dfModTime :: Timestamp
    }
    deriving (Eq, Show)

type DbFileList = [(EntryName, DbFileInfo)]

data CloudFileInfo = CloudFileInfo
    { cfHash :: Hash
    , cfLength :: Length
    , cfModTime :: Timestamp
    , cfVersion :: VersionId
    }
    deriving (Eq, Show)

data CloudFileStatus
    = CloudFile CloudFileInfo
    | CloudDeleteMarker
    deriving Show

type CloudFileList = [(EntryName, CloudFileStatus)]
