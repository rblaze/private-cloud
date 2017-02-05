{-# Language OverloadedStrings, RecordWildCards #-}
module PrivateCloud.LocalDb where

import Database.SQLite.Simple

import PrivateCloud.FileInfo

data DbInfo = DbInfo
    { dbConnection :: Connection
    }

withDatabase :: FilePath -> (DbInfo -> IO a) -> IO a
withDatabase path f =
    withConnection path $ \conn -> do
        withTransaction conn $
            execute_ conn "CREATE TABLE IF NOT EXISTS localFiles (file TEXT PRIMARY KEY NOT NULL, lastSyncedHash TEXT, lastSyncedSize INT, lastSyncedModTime INT)"
        let db = DbInfo { dbConnection = conn }
        f db

getFileList :: DbInfo -> IO DbFileList
getFileList DbInfo{ dbConnection = conn } =
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

putFileInfo :: DbInfo -> EntryName -> DbFileInfo -> IO ()
putFileInfo DbInfo{ dbConnection = conn } (EntryName file) DbFileInfo{..} = withTransaction conn $ do
    let Timestamp ts = dfModTime
    execute conn "INSERT OR REPLACE INTO localFiles (file, lastSyncedHash, lastSyncedSize, lastSyncedModTime) VALUES (?,?,?,?)" (file, hash2text dfHash, dfLength, ts)

deleteFileInfo :: DbInfo -> EntryName -> IO ()
deleteFileInfo DbInfo{ dbConnection = conn } (EntryName file) = withTransaction conn $
    execute conn "DELETE FROM localFiles WHERE file = ?" (Only file)
