{-# Language RecordWildCards #-}
module PrivateCloud.Sync where

import Control.Monad
import System.FilePath

import PrivateCloud.Crypto
import PrivateCloud.DirTree
import PrivateCloud.FileInfo

getLocalChanges :: FilePath -> FileList -> [(FilePath, FileInfo)] -> IO [(FilePath, Maybe FileInfo)]
getLocalChanges _ [] [] = return []
-- handle deleted files
getLocalChanges _ [] srvs = return $ map (\f -> (fst f, Nothing)) srvs
-- handle new files
getLocalChanges root locals [] = forM locals $ \(filename, LocalFileInfo{..}) -> do
    hash <- getFileHash (root </> filename)
    return (filename, Just FileInfo
        { fiHash = hash
        , fiLength = fromIntegral lfLength
        , fiModTime = lfModTime
        })
getLocalChanges root locals@((lname, linfo) : ls) srvs@((sname, sinfo) : ss) =
    case compare lname sname of
        LT -> do    -- new file added
                hash <- getFileHash (root </> lname)
                rest <- getLocalChanges root ls srvs
                let upd = FileInfo
                        { fiHash = hash
                        , fiLength = fromIntegral (lfLength linfo)
                        , fiModTime = lfModTime linfo
                        }
                return $ (lname, Just upd) : rest
        GT -> do    -- file deleted
                rest <- getLocalChanges root locals ss
                return $ (sname, Nothing) : rest
        EQ -> if lfLength linfo == fromIntegral (fiLength sinfo)
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
                    return $ if hash == fiHash sinfo
                        then rest
                        else (lname, Just upd) : rest
