{-# Language OverloadedStrings, RecordWildCards, GeneralizedNewtypeDeriving #-}
module PrivateCloud.LocalDb where

import Control.Exception.Safe
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Database.SQLite.Simple
import System.FilePath

import PrivateCloud.FileInfo

dbName :: FilePath
dbName = ".privatecloud"

newtype DatabaseT m a = DatabaseT (ReaderT Connection m a)
    deriving (Functor, Applicative, Monad, MonadIO)

runDatabaseT :: (MonadMask m, MonadIO m) => FilePath -> DatabaseT m a -> m a
runDatabaseT root (DatabaseT f) =
    bracket
        (liftIO $ open $ root </> dbName)
        (liftIO . close) $
        runReaderT f

connection :: Monad m => DatabaseT m Connection
connection = DatabaseT ask

withDbConnection :: MonadIO m => (Connection -> IO a) -> DatabaseT m a
withDbConnection f = do
    conn <- connection
    liftIO $ f conn

withDbTransaction :: MonadIO m => (Connection -> IO a) -> DatabaseT m a
withDbTransaction f = withDbConnection $ \conn -> withTransaction conn (f conn)

initDatabase :: MonadIO m => DatabaseT m ()
initDatabase = 
    withDbTransaction $ \conn -> do
        execute_ conn "CREATE TABLE IF NOT EXISTS localFiles (file TEXT PRIMARY KEY NOT NULL, lastSyncedHash TEXT, lastSyncedSize INT, lastSyncedModTime INT)"
        execute_ conn "CREATE TABLE IF NOT EXISTS settings (name TEXT PRIMARY KEY NOT NULL, value TEXT)"

writeSetting :: MonadIO m => String -> String -> DatabaseT m ()
writeSetting name value = do
    withDbTransaction $ \conn ->
        execute conn "INSERT OR REPLACE INTO settings (name, value) VALUES (?,?)" (name, value)

readSetting :: MonadIO m => String -> DatabaseT m (Maybe String)
readSetting name = withDbConnection $ \conn -> do
    rows <- query conn "SELECT value FROM settings WHERE name = ?" (Only name)
    case rows of
        [Only value] -> return (Just value)
        _ -> return Nothing

getFileList :: MonadIO m => DatabaseT m DbFileList
getFileList = withDbConnection $ \conn ->
    map convertRow <$> query_ conn "SELECT file, lastSyncedHash, lastSyncedSize, lastSyncedModTime FROM localFiles ORDER BY file"
    where
    convertRow (file, hash, size, ts) =
        ( EntryName file
        , DbFileInfo
            { dfHash = Hash hash
            , dfLength = size
            , dfModTime = Timestamp ts
            }
        )

putFileInfo :: MonadIO m => EntryName -> DbFileInfo -> DatabaseT m ()
putFileInfo (EntryName file) DbFileInfo{..} =
    withDbTransaction $ \conn -> do
        let Timestamp ts = dfModTime
        execute conn "INSERT OR REPLACE INTO localFiles (file, lastSyncedHash, lastSyncedSize, lastSyncedModTime) VALUES (?,?,?,?)" (file, hash2text dfHash, dfLength, ts)

deleteFileInfo :: MonadIO m => EntryName -> DatabaseT m ()
deleteFileInfo (EntryName file) =
    withDbTransaction $ \conn ->
        execute conn "DELETE FROM localFiles WHERE file = ?" (Only file)
