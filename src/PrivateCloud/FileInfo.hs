{-# Language OverloadedStrings, GeneralizedNewtypeDeriving #-}
module PrivateCloud.FileInfo where

import Data.Hashable
import Data.Word
import Data.Time.Clock.POSIX
import System.FilePath
import qualified Data.ByteString as BS
import qualified Data.Text as T

import PrivateCloud.Aws

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

data LocalFileInfo = LocalFileInfo
    { lfLength :: Word64
    , lfModTime :: POSIXTime
    }
    deriving (Eq, Show)

type LocalFileList = [(EntryName, LocalFileInfo)]

data DbFileInfo = DbFileInfo
    { dfHash :: BS.ByteString
    , dfLength :: Word64
    , dfModTime :: POSIXTime
    }
    deriving (Eq, Show)

type DbFileList = [(EntryName, DbFileInfo)]

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

type CloudFileList = [(EntryName, CloudFileStatus)]
