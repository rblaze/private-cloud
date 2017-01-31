{-# Language LambdaCase, RecordWildCards #-}
module PrivateCloud.Sync where

import Control.Monad
import Data.Maybe
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

zipLists :: Ord f => [(f, a)] -> [(f, b)] -> [(f, Maybe a, Maybe b)]
zipLists [] [] = []
zipLists as [] = map (\(f, a) -> (f, Just a, Nothing)) as
zipLists [] bs = map (\(f, b) -> (f, Nothing, Just b)) bs
zipLists as@(a : as') bs@(b : bs') = case compare (fst a) (fst b) of
    EQ -> (fst a, Just $ snd a, Just $ snd b) : zipLists as' bs'
    LT -> (fst a, Just $ snd a, Nothing) : zipLists as' bs
    GT -> (fst b, Nothing, Just $ snd b) : zipLists as bs'

getServerChanges :: [(FilePath, DbFileInfo)] -> [(FilePath, CloudFileStatus)] -> IO [(FilePath, CloudFileUpdate)]
getServerChanges = getServerChanges' $ \filename -> do
    -- file deleted on server
    logServerDelete filename
    return $ Just (filename, CloudDelete)

getRecentServerChanges :: [(FilePath, DbFileInfo)] -> [(FilePath, CloudFileStatus)] -> IO [(FilePath, CloudFileUpdate)]
getRecentServerChanges = getServerChanges' $
    -- file was not updated recently
    -- changes (if any missed) will be picked up by next full check
    const (return Nothing)

getServerChanges' :: (FilePath -> IO (Maybe (FilePath, CloudFileUpdate))) -> [(FilePath, DbFileInfo)] -> [(FilePath, CloudFileStatus)] -> IO [(FilePath, CloudFileUpdate)]
getServerChanges' func dbfiles srvfiles = do
    let files = zipLists dbfiles srvfiles
    changes <- forM files $ \case
        (_, Nothing, Nothing) -> do
            -- should never happen
            criticalM syncLoggerName $ "internal error in sync: two Nothings in zip " ++ show files
            error "internal error"
        (filename, Nothing, Just (CloudFile info)) -> do
            -- new file on server
            logServerNew filename (cfLength info)
            return $ Just (filename, CloudContentChange info)
        (_, Nothing, Just CloudDeleteMarker) ->
            -- file already deleted locally
            return Nothing
        (filename, Just _, Nothing) ->
            -- see calling function
            func filename
        (filename, Just _, Just CloudDeleteMarker) -> do
            -- file deleted on server
            logServerDelete filename
            return $ Just (filename, CloudDelete)
        (filename, Just localinfo, Just (CloudFile cloudinfo))
            | dfLength localinfo == cfLength cloudinfo
                && dfModTime localinfo == cfModTime cloudinfo
                && dfHash localinfo == cfHash cloudinfo -> do
            -- no change
            infoM syncLoggerName $ "#NOCHANGE_SERVER #file " ++ filename
            return Nothing
        (filename, Just localinfo, Just (CloudFile cloudinfo))
            | dfHash localinfo == cfHash cloudinfo -> do
            -- medatada update
            when (dfLength localinfo /= cfLength cloudinfo)
                $ criticalM syncLoggerName
                $ "file size changed but hash remains the same: "
                    ++ filename ++ " " ++ show localinfo
                    ++ " " ++ show cloudinfo
            logServerMetadataChange filename (dfModTime localinfo) (cfModTime cloudinfo)
            return $ Just (filename, CloudMetadataChange cloudinfo)
        (filename, Just localinfo, Just (CloudFile cloudinfo)) -> do
            -- file changed
            logServerChange filename localinfo cloudinfo
            return $ Just (filename, CloudContentChange cloudinfo)

    return $ catMaybes changes
