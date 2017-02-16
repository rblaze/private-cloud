{-# Language OverloadedStrings, RecordWildCards, GeneralizedNewtypeDeriving #-}
module PrivateCloud.Monad where

import Control.Exception.Safe
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Database.SQLite.Simple
import System.FilePath

dbName :: FilePath
dbName = ".privatecloud"

data CloudContext = CloudContext
    { ccConnection :: Connection
    }

newtype PrivateCloudT m a = PrivateCloudT (ReaderT CloudContext m a)
    deriving (Functor, Applicative, Monad, MonadIO)

runPrivateCloudT :: (MonadMask m, MonadIO m) => FilePath -> PrivateCloudT m a -> m a
runPrivateCloudT root (PrivateCloudT f) =
    bracket
        (liftIO $ open $ root </> dbName)
        (liftIO . close) $
        \conn -> runReaderT f CloudContext
            { ccConnection = conn
            }

connection :: Monad m => PrivateCloudT m Connection
connection = PrivateCloudT (asks ccConnection)

