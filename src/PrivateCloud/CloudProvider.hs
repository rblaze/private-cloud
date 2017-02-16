{-# Language TypeFamilies, StandaloneDeriving, FlexibleContexts, GeneralizedNewtypeDeriving, UndecidableInstances #-}
module PrivateCloud.CloudProvider where

import Control.Exception.Safe
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.ByteArray
import Network.AWS
import qualified Data.Text as T

import PrivateCloud.FileInfo

newtype CloudMonad p a = CloudMonad ((ProviderMonad p) a)
deriving instance Functor (ProviderMonad p) => Functor (CloudMonad p)
deriving instance Applicative (ProviderMonad p) => Applicative (CloudMonad p)
deriving instance Monad (ProviderMonad p) => Monad (CloudMonad p)
deriving instance MonadCatch (ProviderMonad p) => MonadCatch (CloudMonad p)
deriving instance MonadIO (ProviderMonad p) => MonadIO (CloudMonad p)
deriving instance MonadThrow (ProviderMonad p) => MonadThrow (CloudMonad p)
deriving instance MonadAWS (ProviderMonad p) => MonadAWS (CloudMonad p)

-- | Cloud services used as a backend for private cloud instance.
class CloudProvider p where
    type ProviderMonad p :: * -> *
    type ProviderContext p :: *

    -- | Run actions in cloud monad with given context.
    runCloud :: MonadResource m => ProviderContext p -> CloudMonad p a -> m a

    -- | Create access credentials with minimal required permissions.
    -- Runs in elevated context.
    -- Elevated context is created in provider and application-dependent way,
    -- outside of the scope of `CloudProvider`
    createCloudInstance :: ByteArray ba => T.Text -> CloudMonad p ba

    -- | Create low-privileged provider context, used to run cloud instance.
    newContext :: p -> IO (ProviderContext p)

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
    downloadFile :: EntryName -> FilePath -> CloudMonad p ()
    -- | Delete file in cloud storage.
    deleteFile :: EntryName -> CloudMonad p ()

    -- | Clean obsolete entries from cloud database and storage.
    cleanupCloud :: CloudMonad p ()
