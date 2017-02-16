{-# Language OverloadedStrings, RecordWildCards #-}
module PrivateCloud.LocalDb where

import Control.Monad.IO.Class
import Database.SQLite.Simple

import PrivateCloud.Monad
import PrivateCloud.FileInfo

withDbConnection :: MonadIO m => (Connection -> IO a) -> PrivateCloudT m a
withDbConnection f = do
    conn <- connection
    liftIO $ f conn

withDbTransaction :: MonadIO m => (Connection -> IO a) -> PrivateCloudT m a
withDbTransaction f = withDbConnection $ \conn -> withTransaction conn (f conn)

initDatabase :: MonadIO m => PrivateCloudT m ()
initDatabase = 
    withDbTransaction $ \conn -> do
        execute_ conn "CREATE TABLE IF NOT EXISTS localFiles (file TEXT PRIMARY KEY NOT NULL, lastSyncedHash TEXT, lastSyncedSize INT, lastSyncedModTime INT)"
        execute_ conn "CREATE TABLE IF NOT EXISTS settings (name TEXT PRIMARY KEY NOT NULL, value TEXT)"

writeSetting :: MonadIO m => String -> String -> PrivateCloudT m ()
writeSetting name value = do
    withDbTransaction $ \conn ->
        execute conn "INSERT OR REPLACE INTO settings (name, value) VALUES (?,?)" (name, value)

readSetting :: MonadIO m => String -> PrivateCloudT m (Maybe String)
readSetting name = withDbConnection $ \conn -> do
    rows <- query conn "SELECT value FROM settings WHERE name = ?" (Only name)
    case rows of
        [Only value] -> return (Just value)
        _ -> return Nothing

getFileList :: MonadIO m => PrivateCloudT m DbFileList
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

putFileInfo :: MonadIO m => EntryName -> DbFileInfo -> PrivateCloudT m ()
putFileInfo (EntryName file) DbFileInfo{..} =
    withDbTransaction $ \conn -> do
        let Timestamp ts = dfModTime
        execute conn "INSERT OR REPLACE INTO localFiles (file, lastSyncedHash, lastSyncedSize, lastSyncedModTime) VALUES (?,?,?,?)" (file, hash2text dfHash, dfLength, ts)

deleteFileInfo :: MonadIO m => EntryName -> PrivateCloudT m ()
deleteFileInfo (EntryName file) =
    withDbTransaction $ \conn ->
        execute conn "DELETE FROM localFiles WHERE file = ?" (Only file)
