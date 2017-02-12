{-# Language OverloadedStrings, RecordWildCards #-}
module PrivateCloud.LocalDb where

import Database.SQLite.Simple

import PrivateCloud.FileInfo
import PrivateCloud.ServiceConfig

initDatabase :: ServiceConfig -> IO ()
initDatabase ServiceConfig{..} = do
    withTransaction scConnection $
        execute_ scConnection "CREATE TABLE IF NOT EXISTS localFiles (file TEXT PRIMARY KEY NOT NULL, lastSyncedHash TEXT, lastSyncedSize INT, lastSyncedModTime INT)"

getFileList :: ServiceConfig -> IO DbFileList
getFileList ServiceConfig{..} = do
    map convertRow <$> query_ scConnection "SELECT file, lastSyncedHash, lastSyncedSize, lastSyncedModTime FROM localFiles ORDER BY file"
    where
    convertRow (file, hash, size, ts) =
        ( EntryName file
        , DbFileInfo
            { dfHash = Hash hash
            , dfLength = size
            , dfModTime = Timestamp ts
            }
        )

putFileInfo :: ServiceConfig -> EntryName -> DbFileInfo -> IO ()
putFileInfo ServiceConfig{..} (EntryName file) DbFileInfo{..} =
    withTransaction scConnection $ do
        let Timestamp ts = dfModTime
        execute scConnection "INSERT OR REPLACE INTO localFiles (file, lastSyncedHash, lastSyncedSize, lastSyncedModTime) VALUES (?,?,?,?)" (file, hash2text dfHash, dfLength, ts)

deleteFileInfo :: ServiceConfig -> EntryName -> IO ()
deleteFileInfo ServiceConfig{..} (EntryName file) =
    withTransaction scConnection $
        execute scConnection "DELETE FROM localFiles WHERE file = ?" (Only file)
