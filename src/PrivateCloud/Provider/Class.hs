{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module PrivateCloud.Provider.Class where

import Data.ByteArray
import Data.Tagged

import PrivateCloud.Provider.Types

newtype CloudMonad p a = CloudMonad ((ProviderMonad p) a)
deriving instance Functor (ProviderMonad p) => Functor (CloudMonad p)
deriving instance Applicative (ProviderMonad p) => Applicative (CloudMonad p)
deriving instance Monad (ProviderMonad p) => Monad (CloudMonad p)

-- | Cloud services used as a backend for private cloud instance.
class CloudProvider p where
    type ProviderMonad p :: * -> *
    type ProviderContext p :: *

    -- | Create a provider context from saved credentials,
    -- used to run cloud instance.
    loadContext :: ByteArrayAccess ba => Tagged p ba -> IO (ProviderContext p)

    -- | Run actions in cloud monad with the given context.
    runCloud :: Tagged p (ProviderContext p) ->
        CloudMonad p a -> IO a

    -- | Update or create file information in the cloud.
    uploadFileInfo :: EntryName -> CloudFileInfo -> CloudMonad p ()
    -- | Update file information in the cloud. The record must exist
    -- and hash value match.
    uploadFileMetadata :: EntryName -> DbFileInfo -> CloudMonad p ()
    -- | Mark file as deleted.
    uploadDeleteMarker :: EntryName -> CloudMonad p ()
    -- | Get the entire cloud database.
    getAllServerFiles :: CloudMonad p CloudFileList
    -- | Get recently updated file entries.
    getRecentServerFiles :: CloudMonad p CloudFileList

    -- | Upload file to cloud storage. Get back its storage id, actual length
    -- and hash of bytes uploaded.
    uploadFile :: FilePath -> CloudMonad p (StorageId, Length, Hash)
    -- | Download file from cloud storage.
    downloadFile :: StorageId -> Hash -> FilePath -> CloudMonad p ()

    -- | Clean obsolete entries from the cloud database and storage.
    cleanupCloud :: CloudMonad p ()
