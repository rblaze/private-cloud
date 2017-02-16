{-# Language OverloadedStrings, RecordWildCards, GeneralizedNewtypeDeriving, StandaloneDeriving #-}
module PrivateCloud.Monad
    ( PrivateCloudT
    , runPrivateCloudT
    , dbName
    , connection
    , exclusions
    ) where

import Control.Exception.Safe
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Database.SQLite.Simple
import Network.AWS
import System.FilePath
import System.FilePath.Glob

dbName :: FilePath
dbName = ".privatecloud"

data CloudContext = CloudContext
    { ccConnection :: Connection
    , ccExclusions :: [Pattern]
    }

newtype PrivateCloudT m a = PrivateCloudT (ReaderT CloudContext m a)
    deriving (Functor, Applicative, Monad, MonadIO)

deriving instance MonadThrow m => MonadThrow (PrivateCloudT m)
deriving instance MonadCatch m => MonadCatch (PrivateCloudT m)
deriving instance MonadAWS m => MonadAWS (PrivateCloudT m)

runPrivateCloudT :: (MonadMask m, MonadIO m) => FilePath -> [Pattern] -> PrivateCloudT m a -> m a
runPrivateCloudT root excls (PrivateCloudT f) =
    bracket
        (liftIO $ open $ root </> dbName)
        (liftIO . close) $
        \conn -> runReaderT f CloudContext
            { ccConnection = conn
            , ccExclusions = excls
            }

connection :: Monad m => PrivateCloudT m Connection
connection = PrivateCloudT (asks ccConnection)

exclusions :: Monad m => PrivateCloudT m [Pattern]
exclusions = PrivateCloudT (asks ccExclusions)
