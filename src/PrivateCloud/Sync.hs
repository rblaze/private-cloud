{-# Language MultiWayIf, LambdaCase, RecordWildCards #-}
module PrivateCloud.Sync where

import Control.Monad
import Data.Maybe
import Data.Time.Clock.POSIX
import Data.Word
import System.FilePath
import System.Log.Logger
import qualified Data.ByteString as BS

import PrivateCloud.Crypto
import PrivateCloud.FileInfo

syncLoggerName :: String
syncLoggerName = "PrivateCloud.Sync"

data FileAction
    = ResolveConflict
        { faFilename :: EntryName
        , faCloudInfo :: CloudFileInfo
        }
    | UpdateCloudFile
        { faFilename :: EntryName
        , faLocalInfo :: LocalFileInfo
        }
    | UpdateCloudMetadata
        { faFilename :: EntryName
        , faLocalInfo :: LocalFileInfo
        , faExpectedHash :: BS.ByteString
        }
    | DeleteCloudFile
        { faFilename :: EntryName
        }
    | UpdateLocalFile
        { faFilename :: EntryName
        , faCloudInfo :: CloudFileInfo
        }
    | UpdateLocalMetadata
        { faFilename :: EntryName
        , faCloudInfo :: CloudFileInfo
        }
    | DeleteLocalFile
        { faFilename :: EntryName
        }
    deriving (Show, Eq)

logConflict :: EntryName -> Word64 -> Word64 -> IO ()
logConflict file localSize cloudSize = do
    noticeM syncLoggerName $ "#CONFLICT #file " ++ show file
        ++ " #localsize " ++ show localSize ++ " #serversize " ++ show cloudSize

logLocalNew :: EntryName -> Word64 -> IO ()
logLocalNew file size = do
    noticeM syncLoggerName $ "#NEW_LOCAL #file " ++ show file ++ " #size " ++ show size

logLocalDelete :: EntryName -> IO ()
logLocalDelete file = noticeM syncLoggerName $ "#DEL_LOCAL #file " ++ show file

logLocalChange :: EntryName -> Word64 -> Word64 -> IO ()
logLocalChange file oldSize newSize = do
    noticeM syncLoggerName $ "#UPD_LOCAL #file " ++ show file ++ " #size " ++ show newSize ++ " #oldsize " ++  show oldSize

logLocalMetadataChange :: EntryName -> POSIXTime -> POSIXTime -> IO ()
logLocalMetadataChange file oldTs newTs =
    noticeM syncLoggerName $ "#UPDMETA_LOCAL #file " ++ show file
        ++ " #ts " ++ show newTs ++ " #oldts " ++  show oldTs

logServerNew :: EntryName -> Word64 -> IO ()
logServerNew file size = do
    noticeM syncLoggerName $ "#NEW_SERVER #file " ++ show file ++ " #size " ++ show size

logServerDelete :: EntryName -> IO ()
logServerDelete file = noticeM syncLoggerName $ "#DEL_SERVER #file " ++ show file

logServerChange :: EntryName -> DbFileInfo -> CloudFileInfo -> IO ()
logServerChange file oldInfo newInfo =
    noticeM syncLoggerName $ "#UPD_SERVER #file " ++ show file
        ++ " #size " ++ show (cfLength newInfo)
        ++ " #oldsize " ++  show (dfLength oldInfo)

logServerMetadataChange :: EntryName -> POSIXTime -> POSIXTime -> IO ()
logServerMetadataChange file oldTs newTs =
    noticeM syncLoggerName $ "#UPDMETA_SERVER #file " ++ show file
        ++ " #ts " ++ show newTs ++ " #oldts " ++  show oldTs

zipLists3 :: Ord f => [(f, a)] -> [(f, b)] -> [(f, c)] -> [(f, Maybe a, Maybe b, Maybe c)]
zipLists3 [] [] [] = []
zipLists3 as [] [] = map (\(f, a) -> (f, Just a, Nothing, Nothing)) as
zipLists3 [] bs [] = map (\(f, b) -> (f, Nothing, Just b, Nothing)) bs
zipLists3 [] [] cs = map (\(f, c) -> (f, Nothing, Nothing, Just c)) cs
zipLists3 as bs cs = (firstName, aval, bval, cval) : zipLists3 as' bs' cs'
    where
    ha = headMay as
    hb = headMay bs
    hc = headMay cs
    firstName = minimum $ catMaybes [fst <$> ha, fst <$> hb, fst <$> hc]
    getVal Nothing xs = (Nothing, xs)
    getVal (Just (f, v)) xs
        | f == firstName = (Just v, tail xs)
        | otherwise = (Nothing, xs)
    (aval, as') = getVal ha as
    (bval, bs') = getVal hb bs
    (cval, cs') = getVal hc cs
    headMay [] = Nothing
    headMay (x:_) = Just x

getFileChanges :: FilePath -> LocalFileList -> DbFileList -> CloudFileList -> IO [FileAction]
getFileChanges root localFiles dbFiles cloudFiles = do
    changes <- forM files $ \case
        (_, Nothing, Nothing, Nothing) -> do
            -- should never happen
            criticalM syncLoggerName $ "internal error in sync: two Nothings in zip " ++ show files
            error "internal error"
        (filename, Nothing, Nothing, Just (CloudFile cloudinfo)) -> do
            -- server file added
            let CloudFileInfo{..} = cloudinfo
            logServerNew filename cfLength
            return $ Just $ UpdateLocalFile filename cloudinfo
        (_, Nothing, Nothing, Just CloudDeleteMarker) ->
            -- server file added and deleted, effectively no change
            return Nothing
        (filename, Just localinfo@LocalFileInfo{..}, Nothing, Nothing) -> do
            -- local file added
            logLocalNew filename lfLength
            return $ Just $ UpdateCloudFile filename localinfo
        (filename, Just localinfo@LocalFileInfo{..}, Nothing, Just CloudDeleteMarker) -> do
            -- local file added, server file added and deleted
            -- uploading local file
            logLocalNew filename lfLength
            return $ Just $ UpdateCloudFile filename localinfo
        (filename, Just localinfo@LocalFileInfo{..}, Nothing, Just (CloudFile cloudinfo)) -> do
            -- local file added, server file added
            -- check if files are the same
            let CloudFileInfo{..} = cloudinfo
            filesDiffer <- isLocalFileChanged filename lfLength cfLength cfHash
            if filesDiffer
                then do
                    -- files differ
                    logConflict filename lfLength cfLength
                    return $ Just $ ResolveConflict filename cloudinfo
                else
                    -- somehow files are the same, maybe we just lost local db
                    -- just set latest timestamp everywhere
                    syncModTimes filename cfHash localinfo cloudinfo
        (filename, Nothing, Just DbFileInfo{..}, Nothing) -> do
            -- both deleted
            -- delete local db entry
            logLocalDelete filename
            return $ Just $ DeleteLocalFile filename
        (filename, Nothing, Just DbFileInfo{..}, Just CloudDeleteMarker) -> do
            -- both deleted again
            -- delete local db entry
            logLocalDelete filename
            return $ Just $ DeleteLocalFile filename
        (filename, Nothing, Just DbFileInfo{..}, Just (CloudFile cloudinfo)) -> do
            -- local file deleted
            -- check if cloud version is the same as the local was
            let CloudFileInfo{..} = cloudinfo
            if dfHash == cfHash
                then do
                    -- local deleted, cloud unchanged
                    -- delete cloud file
                    logLocalDelete filename
                    return $ Just $ DeleteCloudFile filename
                else do
                    -- local deleted, but cloud has newer version
                    -- download it
                    noticeM syncLoggerName $ "#UPD_SERVER_DELETE_LOCAL #file " ++ show filename ++ " #size " ++ show cfLength
                    return $ Just $ UpdateLocalFile filename cloudinfo
        (filename, Just localinfo@LocalFileInfo{..}, Just DbFileInfo{..}, Nothing) -> do
            -- file deleted on server
            -- check if it was modified locally
            fileChanged <- isLocalFileChanged filename lfLength dfLength dfHash
            if fileChanged
                then do
                    -- local file changed, upload to cloud
                    logLocalChange filename dfLength lfLength
                    return $ Just $ UpdateCloudFile filename localinfo
                else do
                    -- no local change, delete it
                    logServerDelete filename
                    return $ Just $ DeleteLocalFile filename
        (filename, Just localinfo@LocalFileInfo{..}, Just DbFileInfo{..}, Just CloudDeleteMarker) -> do
            -- file deleted on server
            -- check if it was modified locally
            fileChanged <- isLocalFileChanged filename lfLength dfLength dfHash
            if fileChanged
                then do
                    -- local file changed, upload to cloud
                    logLocalChange filename dfLength lfLength
                    return $ Just $ UpdateCloudFile filename localinfo
                else do
                    -- no local change, delete it
                    logServerDelete filename
                    return $ Just $ DeleteLocalFile filename
        (filename, Just localinfo@LocalFileInfo{..}, Just dbinfo@DbFileInfo{..}, Just (CloudFile cloudinfo)) -> do
            let CloudFileInfo{..} = cloudinfo
            -- let's not check file hash every time
            -- pretend that if size and mod time are the same, there were no changes
            localFileUpdated <- if lfLength == dfLength && lfModTime == dfModTime
                then return False
                else do
                    localHash <- getFileHash (root </> entry2path filename)
                    return $ localHash /= dfHash
            let localMetadataUpdated = lfModTime /= dfModTime
            let cloudFileUpdated = cfHash /= dfHash
            let cloudMetadataUpdated = cfModTime /= dfModTime
            if | localFileUpdated && cloudFileUpdated -> do
                    -- both files updated
                    logConflict filename lfLength cfLength
                    return $ Just $ ResolveConflict filename cloudinfo
               | localFileUpdated -> do
                    -- local file changed, upload to cloud
                    logLocalChange filename dfLength lfLength
                    return $ Just $ UpdateCloudFile filename localinfo
               | cloudFileUpdated -> do
                    -- cloud file changed, download it
                    logServerChange filename dbinfo cloudinfo
                    return $ Just $ UpdateLocalFile filename cloudinfo
               | otherwise ->
                    -- no content update, check for metadata updates
                    if (not localMetadataUpdated) && (not cloudMetadataUpdated)
                        then return Nothing
                        else syncModTimes filename dfHash localinfo cloudinfo

    return $ catMaybes changes
    where
    files = zipLists3 localFiles dbFiles cloudFiles
    isLocalFileChanged filename newLength oldLength oldHash
        | newLength /= oldLength = return True
        | otherwise = do
            localHash <- getFileHash (root </> entry2path filename)
            return $ localHash /= oldHash
    syncModTimes filename hash localinfo cloudinfo = do
        let LocalFileInfo{..} = localinfo
        let CloudFileInfo{..} = cloudinfo
        if lfModTime > cfModTime
            then do
                logLocalMetadataChange filename cfModTime lfModTime
                return $ Just $ UpdateCloudMetadata filename localinfo hash
            else do
                logServerMetadataChange filename lfModTime cfModTime
                return $ Just $ UpdateLocalMetadata filename cloudinfo
