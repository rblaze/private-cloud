{-# Language OverloadedStrings #-}
module PrivateCloud.LocalDb
    ( module PrivateCloud.LocalDb
    , Connection
    , withConnection
    ) where

import Database.SQLite.Simple
import Data.Word
import qualified Data.ByteString as BS

initDatabase :: Connection -> IO ()
initDatabase conn = withTransaction conn $
    execute_ conn "CREATE TABLE IF NOT EXISTS localFiles (file TEXT PRIMARY KEY NOT NULL, lastSyncedHash TEXT, lastSyncedSize INT)"

getFileInfo :: Connection -> FilePath -> IO (Maybe (BS.ByteString, Word64))
getFileInfo conn file = do
    result <- query conn "SELECT lastSyncedHash, lastSyncedSize FROM localFiles WHERE file = ?" (Only file)
    case result of
        [v@(_, _)] -> return $ Just v
        [] -> return Nothing
        _ -> error $ "Internal error: db response format violation, " ++ show result

putFileInfo :: Connection -> FilePath -> BS.ByteString -> Word64 -> IO ()
putFileInfo conn file hash size = withTransaction conn $
    execute conn "INSERT OR REPLACE INTO localFiles (file, lastSyncedHash, lastSyncedSize) VALUES (?,?,?)" (file, hash, size)

deleteFileInfo :: Connection -> FilePath -> IO ()
deleteFileInfo conn file = withTransaction conn $
    execute conn "DELETE FROM localFiles WHERE file = ?" (Only file)
