{-# Language RecordWildCards #-}
module PrivateCloud.Sync where

import Control.Monad
import Data.Time.Clock.POSIX
import System.FilePath
import System.Log.Logger

import PrivateCloud.Crypto
import PrivateCloud.DirTree
import PrivateCloud.FileInfo

data LocalFileUpdate
    = LocalContentChange DbFileInfo
    | LocalMetadataChange DbFileInfo
    | LocalDelete
    deriving (Show, Eq)

data CloudFileUpdate
    = CloudContentChange CloudFileInfo
    | CloudMetadataChange CloudFileInfo
    | CloudDelete
    deriving (Show, Eq)

syncLoggerName :: String
syncLoggerName = "PrivateCloud.Sync"

logLocalNew :: (Show a, Integral a) => FilePath -> a -> IO ()
logLocalNew file size = do
    let _ = fromIntegral size :: Int      -- suppress "unused constraint" warning
    noticeM syncLoggerName $ "#NEW_LOCAL #file " ++ file ++ " #size " ++ show size

logLocalDelete :: FilePath -> IO ()
logLocalDelete file = noticeM syncLoggerName $ "#DEL_LOCAL #file " ++ file

logLocalChange :: (Show a, Show b, Integral a, Integral b) => FilePath -> a -> b -> IO ()
logLocalChange file oldSize newSize = do
    let _ = fromIntegral oldSize :: Int
    let _ = fromIntegral newSize :: Int
    noticeM syncLoggerName $ "#UPD_LOCAL #file " ++ file ++ " #size " ++ show newSize ++ " #oldsize " ++  show oldSize

logLocalMetadataChange :: FilePath -> POSIXTime -> POSIXTime -> IO ()
logLocalMetadataChange file oldTs newTs =
    noticeM syncLoggerName $ "#UPDMETA_LOCAL #file " ++ file
        ++ " #ts " ++ show newTs ++ " #oldts " ++  show oldTs


logServerNew :: (Show a, Integral a) => FilePath -> a -> IO ()
logServerNew file size = do
    let _ = fromIntegral size :: Int      -- suppress "unused constraint" warning
    noticeM syncLoggerName $ "#NEW_SERVER #file " ++ file ++ " #size " ++ show size

logServerDelete :: FilePath -> IO ()
logServerDelete file = noticeM syncLoggerName $ "#DEL_SERVER #file " ++ file

logServerChange :: FilePath -> DbFileInfo -> CloudFileInfo -> IO ()
logServerChange file oldInfo newInfo =
    noticeM syncLoggerName $ "#UPD_SERVER #file " ++ file
        ++ " #size " ++ show (cfLength newInfo)
        ++ " #oldsize " ++  show (dfLength oldInfo)

logServerMetadataChange :: FilePath -> POSIXTime -> POSIXTime -> IO ()
logServerMetadataChange file oldTs newTs =
    noticeM syncLoggerName $ "#UPDMETA_SERVER #file " ++ file
        ++ " #ts " ++ show newTs ++ " #oldts " ++  show oldTs

getLocalChanges :: FilePath -> FileList -> [(FilePath, DbFileInfo)] -> IO [(FilePath, LocalFileUpdate)]
getLocalChanges _ [] [] = return []
-- handle deleted files
getLocalChanges _ [] dbs = forM dbs $ \(filename, _) -> do
    logLocalDelete filename
    return (filename, LocalDelete)
-- handle new files
getLocalChanges root locals [] = forM locals $ \(filename, LocalFileInfo{..}) -> do
    hash <- getFileHash (root </> filename)
    logLocalNew filename lfLength
    return (filename, LocalContentChange DbFileInfo
        { dfHash = hash
        , dfLength = fromIntegral lfLength
        , dfModTime = lfModTime
        })
getLocalChanges root locals@((lname, linfo) : ls) dbs@((dbname, dbinfo) : ds) =
    case compare lname dbname of
        LT -> do    -- new file added
                logLocalNew lname (lfLength linfo)
                hash <- getFileHash (root </> lname)
                rest <- getLocalChanges root ls dbs
                let upd = DbFileInfo
                        { dfHash = hash
                        , dfLength = fromIntegral (lfLength linfo)
                        , dfModTime = lfModTime linfo
                        }
                return $ (lname, LocalContentChange upd) : rest
        GT -> do    -- file deleted
                logLocalDelete dbname
                rest <- getLocalChanges root locals ds
                return $ (dbname, LocalDelete) : rest
        EQ -> do
                infoM syncLoggerName $ "#CHK #file " ++ lname
                if lfLength linfo == fromIntegral (dfLength dbinfo)
                    && lfModTime linfo == dfModTime dbinfo
                then getLocalChanges root ls ds     -- no changes
                else do
                    hash <- getFileHash (root </> lname)
                    upd <- if hash == dfHash dbinfo
                            then do
                                logLocalMetadataChange lname (dfModTime dbinfo) (lfModTime linfo)
                                return $ LocalMetadataChange $ dbinfo
                                    { dfModTime = lfModTime linfo
                                    }
                            else do
                                logLocalChange lname (dfLength dbinfo) (lfLength linfo)
                                return $ LocalContentChange DbFileInfo
                                    { dfHash = hash
                                    , dfLength = fromIntegral (lfLength linfo)
                                    , dfModTime = lfModTime linfo
                                    }
                    rest <- getLocalChanges root ls ds
                    return $ (lname, upd) : rest

getServerChanges :: [(FilePath, DbFileInfo)] -> [(FilePath, CloudFileInfo)] -> IO [(FilePath, CloudFileUpdate)]
getServerChanges [] [] = return []
-- handle new files
getServerChanges [] srvs = forM srvs $ \(filename, info) -> do
    logServerNew filename (cfLength info)
    return (filename, CloudContentChange info)
-- handle deleted files
getServerChanges dbs [] = forM dbs $ \(filename, _) -> do
    logServerDelete filename
    return (filename, CloudDelete)
getServerChanges dbs@((dbname, dbinfo) : ds) srvs@((sname, sinfo) : ss) =
    case compare dbname sname of
        GT -> do    -- new file added
                logServerNew sname (cfLength sinfo)
                rest <- getServerChanges dbs ss
                return $ (sname, CloudContentChange sinfo) : rest
        LT -> do    -- file deleted
                logServerDelete dbname
                rest <- getServerChanges ds srvs
                return $ (dbname, CloudDelete) : rest
        EQ -> do
                infoM syncLoggerName $ "#CHKSRV #file " ++ dbname
                if dfLength dbinfo == cfLength sinfo
                        && dfModTime dbinfo == cfModTime sinfo
                        && dfHash dbinfo == cfHash sinfo
                    then getServerChanges ds ss     -- no changes
                    else do
                        upd <- if dfHash dbinfo == cfHash sinfo
                            then do
                                when (dfLength dbinfo /= cfLength sinfo)
                                    $ criticalM syncLoggerName
                                    $ "file size changed but hash remains the same: "
                                        ++ dbname ++ " " ++ show dbinfo
                                        ++ " " ++ show sinfo
                                logServerMetadataChange dbname (dfModTime dbinfo) (cfModTime sinfo)
                                return $ CloudMetadataChange sinfo
                            else do
                                logServerChange dbname dbinfo sinfo
                                return $ CloudContentChange sinfo
                        rest <- getServerChanges ds ss
                        return $ (sname, upd) : rest
