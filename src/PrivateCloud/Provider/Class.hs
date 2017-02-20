{-# Language TypeFamilies, StandaloneDeriving, GeneralizedNewtypeDeriving, UndecidableInstances #-}
module PrivateCloud.Provider.Class where

import Control.Monad.IO.Class
import Data.ByteArray
import Data.Tagged

import PrivateCloud.Provider.FileInfo

newtype CloudMonad p a = CloudMonad ((ProviderMonad p) a)
deriving instance Functor (ProviderMonad p) => Functor (CloudMonad p)
deriving instance Applicative (ProviderMonad p) => Applicative (CloudMonad p)
deriving instance Monad (ProviderMonad p) => Monad (CloudMonad p)

-- | Cloud services used as a backend for private cloud instance.
class CloudProvider p where
    type ProviderMonad p :: * -> *
    type ProviderContext p :: *

    -- | Create provider context from saved credentials, used to run cloud instance.
    newContext :: ByteArray ba => Tagged p ba -> IO (ProviderContext p)

    -- | Run actions in cloud monad with given context.
    runCloud :: MonadIO m => Tagged p (ProviderContext p) -> CloudMonad p a -> m a

    -- | Update or create file information in cloud.
    uploadFileInfo :: EntryName -> CloudFileInfo -> CloudMonad p ()
    -- | Update file information in cloud. Record must exist and hash value match.
    uploadFileMetadata :: EntryName -> DbFileInfo -> CloudMonad p ()
    -- | Mark file as deleted.
    uploadDeleteMarker :: EntryName -> CloudMonad p ()
    -- | Get entire cloud database.
    getAllServerFiles :: CloudMonad p CloudFileList
    -- | Get recently updated file entries.
    getRecentServerFiles :: CloudMonad p CloudFileList

    -- | Upload file to cloud storage. Get back it's version id and actual
    -- length and hash of bytes uploaded.
    uploadFile :: EntryName -> FilePath -> CloudMonad p (VersionId, Length, Hash)
    -- | Download file from cloud storage.
    downloadFile :: EntryName -> VersionId -> FilePath -> CloudMonad p ()
    -- | Delete file in cloud storage.
    -- deleteFile :: EntryName -> CloudMonad p ()

    -- | Clean obsolete entries from cloud database and storage.
    cleanupCloud :: CloudMonad p ()