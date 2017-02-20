{-# Language OverloadedStrings, RecordWildCards #-}
module PrivateCloud.LocalDb
    ( deleteFileInfo
    , getFileList
    , putFileInfo
    ) where

import Control.Monad.IO.Class
import Database.SQLite.Simple

import PrivateCloud.Monad
import PrivateCloud.FileInfo

withDbConnection :: (Connection -> IO a) -> PrivateCloud p a
withDbConnection f = do
    conn <- connection
    liftIO $ f conn

withDbTransaction :: (Connection -> IO a) -> PrivateCloud p a
withDbTransaction f = withDbConnection $ \conn -> withTransaction conn (f conn)

getFileList :: PrivateCloud p DbFileList
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

putFileInfo :: EntryName -> DbFileInfo -> PrivateCloud p ()
putFileInfo (EntryName file) DbFileInfo{..} =
    withDbTransaction $ \conn -> do
        let Timestamp ts = dfModTime
        execute conn "INSERT OR REPLACE INTO localFiles (file, lastSyncedHash, lastSyncedSize, lastSyncedModTime) VALUES (?,?,?,?)" (file, hash2text dfHash, dfLength, ts)

deleteFileInfo :: EntryName -> PrivateCloud p ()
deleteFileInfo (EntryName file) =
    withDbTransaction $ \conn ->
        execute conn "DELETE FROM localFiles WHERE file = ?" (Only file)
