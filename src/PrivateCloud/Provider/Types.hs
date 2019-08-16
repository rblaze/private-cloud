{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module PrivateCloud.Provider.Types where

import Data.ByteArray.Encoding (convertToBase, Base(..))
import Data.Hashable
import Data.Int
import Data.Text.Buildable
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Word
import Foreign.C.Types
import System.FilePath
import System.Posix.Types
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-- | Type for filenames used in database.
-- Path separator is always /, no matter what host os we are on.
newtype EntryName = EntryName T.Text
    deriving (Eq, Ord, Show, Hashable)

path2entry :: FilePath -> EntryName
path2entry = EntryName . T.intercalate "/" . map T.pack . splitDirectories

entry2path :: EntryName -> FilePath
entry2path (EntryName entry) = joinPath $ map T.unpack $ T.splitOn "/" entry

(<//>) :: EntryName -> EntryName -> EntryName
(EntryName "") <//> next = next
(EntryName base) <//> (EntryName next) = EntryName $ T.concat [base, "/", next]

entryFile :: EntryName -> FilePath
entryFile (EntryName entry) = T.unpack $ T.takeWhileEnd (/= '/') entry

printEntry :: EntryName -> String
printEntry (EntryName entry) = T.unpack entry

-- | Type for file's timestamp
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

-- | Type for storage object id
newtype StorageId = StorageId { storageid2text :: T.Text }
    deriving (Eq, Hashable)

instance Show StorageId where
    show (StorageId v) = show v

-- | Type for file hash
newtype Hash = Hash { hash2text :: T.Text }
    deriving Eq

instance Show Hash where
    show (Hash h) = show h

encodeHash :: BS.ByteString -> Hash
encodeHash = Hash . T.decodeUtf8 . convertToBase Base64

-- other types

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
    , cfStorageId :: StorageId
    }
    deriving (Eq, Show)

data CloudFileStatus
    = CloudFile CloudFileInfo
    | CloudDeleteMarker
    deriving Show

type CloudFileList = [(EntryName, CloudFileStatus)]
