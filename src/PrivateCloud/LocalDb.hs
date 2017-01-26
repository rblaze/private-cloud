{-# Language OverloadedStrings, RecordWildCards #-}
module PrivateCloud.LocalDb where

import Database.SQLite.Simple
import Foreign.C.Types

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

getFileList :: DbInfo -> IO [(FilePath, DbFileInfo)]
getFileList DbInfo{ dbConnection = conn } =
    map convertRow <$> query_ conn "SELECT file, lastSyncedHash, lastSyncedSize, lastSyncedModTime FROM localFiles ORDER BY file"
    where
    convertRow (file, hash, size, ts) =
        ( file
        , DbFileInfo
            { dfHash = hash
            , dfLength = size
            , dfModTime = CTime ts
            }
        )

putFileInfo :: DbInfo -> FilePath -> DbFileInfo -> IO ()
putFileInfo DbInfo{ dbConnection = conn } file DbFileInfo{..} = withTransaction conn $ do
    let CTime ts = dfModTime
    execute conn "INSERT OR REPLACE INTO localFiles (file, lastSyncedHash, lastSyncedSize, lastSyncedModTime) VALUES (?,?,?,?)" (file, dfHash, dfLength, ts)

deleteFileInfo :: DbInfo -> FilePath -> IO ()
deleteFileInfo DbInfo{ dbConnection = conn } file = withTransaction conn $
    execute conn "DELETE FROM localFiles WHERE file = ?" (Only file)
