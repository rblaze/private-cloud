{-# Language RecordWildCards #-}
module PrivateCloud.Sync where

import Control.Monad
import System.FilePath
import System.Log.Logger

import PrivateCloud.Crypto
import PrivateCloud.DirTree
import PrivateCloud.FileInfo

syncLoggerName :: String
syncLoggerName = "PrivateCloud.Sync"

logNew :: (Show a, Integral a) => FilePath -> a -> IO ()
logNew file size = do
    let _ = (fromIntegral size) :: Int      -- suppress "unused constraint" warning
    infoM syncLoggerName $ "#NEW #file " ++ file ++ " #size " ++ show size

logDelete :: FilePath -> IO ()
logDelete file = infoM syncLoggerName $ "#DEL #file " ++ file

logChange :: (Show a, Show b, Integral a, Integral b) => FilePath -> a -> b -> IO ()
logChange file oldSize newSize = do
    let _ = (fromIntegral oldSize) :: Int
    let _ = (fromIntegral newSize) :: Int
    infoM syncLoggerName $ "#UPD #file " ++ file ++ " #size " ++ show newSize ++ " #oldsize " ++  show oldSize

getLocalChanges :: FilePath -> FileList -> [(FilePath, FileInfo)] -> IO [(FilePath, Maybe FileInfo)]
getLocalChanges _ [] [] = return []
-- handle deleted files
getLocalChanges _ [] srvs = forM srvs $ \(filename, _) -> do
    logDelete filename
    return (filename, Nothing)
-- handle new files
getLocalChanges root locals [] = forM locals $ \(filename, LocalFileInfo{..}) -> do
    hash <- getFileHash (root </> filename)
    logNew filename lfLength
    return (filename, Just FileInfo
        { fiHash = hash
        , fiLength = fromIntegral lfLength
        , fiModTime = lfModTime
        })
getLocalChanges root locals@((lname, linfo) : ls) srvs@((sname, sinfo) : ss) =
    case compare lname sname of
        LT -> do    -- new file added
                logNew lname (lfLength linfo)
                hash <- getFileHash (root </> lname)
                rest <- getLocalChanges root ls srvs
                let upd = FileInfo
                        { fiHash = hash
                        , fiLength = fromIntegral (lfLength linfo)
                        , fiModTime = lfModTime linfo
                        }
                return $ (lname, Just upd) : rest
        GT -> do    -- file deleted
                logDelete sname
                rest <- getLocalChanges root locals ss
                return $ (sname, Nothing) : rest
        EQ -> do
                debugM syncLoggerName $ "#CHK #file " ++ lname
                if lfLength linfo == fromIntegral (fiLength sinfo)
                    && lfModTime linfo == fiModTime sinfo
                then getLocalChanges root ls ss     -- no changes
                else do
                    hash <- getFileHash (root </> lname)
                    rest <- getLocalChanges root ls ss
                    let upd = FileInfo
                            { fiHash = hash
                            , fiLength = fromIntegral (lfLength linfo)
                            , fiModTime = lfModTime linfo
                            }
                    if hash == fiHash sinfo
                        then return rest
                        else do
                            logChange lname (fiLength sinfo) (lfLength linfo)
                            return $ (lname, Just upd) : rest
