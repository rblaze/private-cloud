{-# Language OverloadedStrings, RecordWildCards, GeneralizedNewtypeDeriving, StandaloneDeriving, TypeFamilies, ScopedTypeVariables, LambdaCase #-}
module PrivateCloud.Monad
    ( PrivateCloud
    , cloudId
    , cloudIdSetting
    , connection
    , context
    , dbName
    , exclusions
    , initCloudSettings
    , rootDir
    , runPrivateCloud
    ) where

import Control.Exception.Safe
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Database.SQLite.Simple
import Data.ByteArray
import Data.Tagged
import System.FilePath
import System.FilePath.Glob
import qualified Data.Text as T

import PrivateCloud.CloudProvider
import PrivateCloud.Exception

dbName :: FilePath
dbName = ".privatecloud"

cloudIdSetting :: String
cloudIdSetting = "cloudid"

data CloudContext p = CloudContext
    { ccConnection :: Connection
    , ccExclusions :: [Pattern]
    , ccCloudId :: T.Text
    , ccRoot :: FilePath
    , ccContext :: Tagged p (ProviderContext p)
    }

newtype PrivateCloud p a = PrivateCloud (ReaderT (CloudContext p) IO a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch)

runPrivateCloud :: forall ba p a. (ByteArray ba, CloudProvider p) => FilePath -> [Pattern] -> (T.Text -> IO (Maybe ba)) -> PrivateCloud p a -> IO a
runPrivateCloud root excls getCreds (PrivateCloud f) =
    withConnection (root </> dbName) $ \conn -> do
        cloudid <- readSetting conn cloudIdSetting >>= \case
            Nothing -> throw $ ConfigurationError "No saved cloudid found"
            Just v -> return v
        creds <- getCreds cloudid >>= \case
            Nothing -> throw $ ConfigurationError "No saved credential found"
            Just v -> return v
        ctx <- newContext (Tagged creds :: Tagged p ba)
        runReaderT f CloudContext
            { ccConnection = conn
            , ccExclusions = excls
            , ccCloudId = cloudid
            , ccRoot = root
            , ccContext = Tagged ctx
            }

initCloudSettings :: FilePath -> T.Text -> IO ()
initCloudSettings root cloudid = do
    withConnection (root </> dbName) $ \conn ->
        withTransaction conn $ do
            execute_ conn "CREATE TABLE localFiles (file TEXT PRIMARY KEY NOT NULL, lastSyncedHash TEXT, lastSyncedSize INT, lastSyncedModTime INT)"
            execute_ conn "CREATE TABLE settings (name TEXT PRIMARY KEY NOT NULL, value TEXT)"
            execute conn "INSERT INTO settings (name, value) VALUES (?,?)" (cloudIdSetting, cloudid)

readSetting :: Connection -> String -> IO (Maybe T.Text)
readSetting conn name = do
    rows <- query conn "SELECT value FROM settings WHERE name = ?" (Only name)
    case rows of
        [Only value] -> return (Just value)
        _ -> return Nothing

connection :: PrivateCloud p Connection
connection = PrivateCloud (asks ccConnection)

exclusions :: PrivateCloud p [Pattern]
exclusions = PrivateCloud (asks ccExclusions)

cloudId :: PrivateCloud p T.Text
cloudId = PrivateCloud (asks ccCloudId)

rootDir :: PrivateCloud p FilePath
rootDir = PrivateCloud (asks ccRoot)

context :: CloudProvider p => PrivateCloud p (Tagged p (ProviderContext p))
context = PrivateCloud (asks ccContext)
