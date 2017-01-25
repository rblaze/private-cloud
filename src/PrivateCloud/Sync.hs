{-# Language RecordWildCards #-}
module PrivateCloud.Sync where

import Control.Monad
import System.FilePath
import System.Log.Logger
import System.Posix.Types

import PrivateCloud.Crypto
import PrivateCloud.DirTree
import PrivateCloud.FileInfo

data FileUpdate
    = ContentChange FileInfo
    | MetadataOnlyChange FileInfo
    | Deleted
    deriving (Show, Eq)

syncLoggerName :: String
syncLoggerName = "PrivateCloud.Sync"

logLocalNew :: (Show a, Integral a) => FilePath -> a -> IO ()
logLocalNew file size = do
    let _ = (fromIntegral size) :: Int      -- suppress "unused constraint" warning
    noticeM syncLoggerName $ "#NEW_LOCAL #file " ++ file ++ " #size " ++ show size

logLocalDelete :: FilePath -> IO ()
logLocalDelete file = noticeM syncLoggerName $ "#DEL_LOCAL #file " ++ file

logLocalChange :: (Show a, Show b, Integral a, Integral b) => FilePath -> a -> b -> IO ()
logLocalChange file oldSize newSize = do
    let _ = (fromIntegral oldSize) :: Int
    let _ = (fromIntegral newSize) :: Int
    noticeM syncLoggerName $ "#UPD_LOCAL #file " ++ file ++ " #size " ++ show newSize ++ " #oldsize " ++  show oldSize

logLocalMetadataChange :: FilePath -> EpochTime -> EpochTime -> IO ()
logLocalMetadataChange file oldTs newTs = do
    noticeM syncLoggerName $ "#UPDMETA_LOCAL #file " ++ file
        ++ " #ts " ++ show newTs ++ " #oldts " ++  show oldTs


logServerNew :: (Show a, Integral a) => FilePath -> a -> IO ()
logServerNew file size = do
    let _ = (fromIntegral size) :: Int      -- suppress "unused constraint" warning
    noticeM syncLoggerName $ "#NEW_SERVER #file " ++ file ++ " #size " ++ show size

logServerDelete :: FilePath -> IO ()
logServerDelete file = noticeM syncLoggerName $ "#DEL_SERVER #file " ++ file

logServerChange :: FilePath -> FileInfo -> FileInfo -> IO ()
logServerChange file oldInfo newInfo = do
    noticeM syncLoggerName $ "#UPD_SERVER #file " ++ file
        ++ " #size " ++ show (fiLength newInfo)
        ++ " #oldsize " ++  show (fiLength oldInfo)

logServerMetadataChange :: FilePath -> EpochTime -> EpochTime -> IO ()
logServerMetadataChange file oldTs newTs = do
    noticeM syncLoggerName $ "#UPDMETA_SERVER #file " ++ file
        ++ " #ts " ++ show newTs ++ " #oldts " ++  show oldTs

getLocalChanges :: FilePath -> FileList -> [(FilePath, FileInfo)] -> IO [(FilePath, FileUpdate)]
getLocalChanges _ [] [] = return []
-- handle deleted files
getLocalChanges _ [] dbs = forM dbs $ \(filename, _) -> do
    logLocalDelete filename
    return (filename, Deleted)
-- handle new files
getLocalChanges root locals [] = forM locals $ \(filename, LocalFileInfo{..}) -> do
    hash <- getFileHash (root </> filename)
    logLocalNew filename lfLength
    return (filename, ContentChange $ FileInfo
        { fiHash = hash
        , fiLength = fromIntegral lfLength
        , fiModTime = lfModTime
        })
getLocalChanges root locals@((lname, linfo) : ls) dbs@((dbname, dbinfo) : ds) =
    case compare lname dbname of
        LT -> do    -- new file added
                logLocalNew lname (lfLength linfo)
                hash <- getFileHash (root </> lname)
                rest <- getLocalChanges root ls dbs
                let upd = FileInfo
                        { fiHash = hash
                        , fiLength = fromIntegral (lfLength linfo)
                        , fiModTime = lfModTime linfo
                        }
                return $ (lname, ContentChange upd) : rest
        GT -> do    -- file deleted
                logLocalDelete dbname
                rest <- getLocalChanges root locals ds
                return $ (dbname, Deleted) : rest
        EQ -> do
                infoM syncLoggerName $ "#CHK #file " ++ lname
                if lfLength linfo == fromIntegral (fiLength dbinfo)
                    && lfModTime linfo == fiModTime dbinfo
                then getLocalChanges root ls ds     -- no changes
                else do
                    hash <- getFileHash (root </> lname)
                    upd <- if hash == fiHash dbinfo
                            then do
                                logLocalMetadataChange lname (fiModTime dbinfo) (lfModTime linfo)
                                return $ MetadataOnlyChange $ dbinfo
                                    { fiModTime = lfModTime linfo
                                    }
                            else do
                                logLocalChange lname (fiLength dbinfo) (lfLength linfo)
                                return $ ContentChange $ FileInfo
                                    { fiHash = hash
                                    , fiLength = fromIntegral (lfLength linfo)
                                    , fiModTime = lfModTime linfo
                                    }
                    rest <- getLocalChanges root ls ds
                    return $ (lname, upd) : rest

getServerChanges :: [(FilePath, FileInfo)] -> [(FilePath, FileInfo)] -> IO [(FilePath, FileUpdate)]
getServerChanges [] [] = return []
-- handle new files
getServerChanges [] srvs = forM srvs $ \(filename, info) -> do
    logServerNew filename (fiLength info)
    return (filename, ContentChange info)
-- handle deleted files
getServerChanges dbs [] = forM dbs $ \(filename, _) -> do
    logServerDelete filename
    return (filename, Deleted)
getServerChanges dbs@((dbname, dbinfo) : ds) srvs@((sname, sinfo) : ss) =
    case compare dbname sname of
        GT -> do    -- new file added
                logServerNew sname (fiLength sinfo)
                rest <- getServerChanges dbs ss
                return $ (sname, ContentChange sinfo) : rest
        LT -> do    -- file deleted
                logServerDelete dbname
                rest <- getServerChanges ds srvs
                return $ (dbname, Deleted) : rest
        EQ -> do
                infoM syncLoggerName $ "#CHKSRV #file " ++ dbname
                if dbinfo == sinfo
                    then getServerChanges ds ss     -- no changes
                    else do
                        upd <- if fiHash dbinfo == fiHash sinfo
                            then do
                                when (fiLength dbinfo /= fiLength sinfo)
                                    $ criticalM syncLoggerName
                                    $ "file size changed but hash remains the same: "
                                        ++ dbname ++ " " ++ show dbinfo
                                        ++ " " ++ show sinfo
                                logServerMetadataChange dbname (fiModTime dbinfo) (fiModTime sinfo)
                                return $ MetadataOnlyChange sinfo
                            else do
                                logServerChange dbname dbinfo sinfo
                                return $ ContentChange sinfo
                        rest <- getServerChanges ds ss
                        return $ (sname, upd) : rest
