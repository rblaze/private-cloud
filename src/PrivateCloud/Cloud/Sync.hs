{-# Language MultiWayIf, LambdaCase, RecordWildCards #-}
module PrivateCloud.Cloud.Sync
    ( FileAction(..)
    , getAllFileChanges
    , getRecentFileChanges
    , zipLists3
    ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Data.Word
import System.FilePath
import System.FilePath.Glob
import System.Log.Logger

import PrivateCloud.Cloud.Crypto
import PrivateCloud.Cloud.Monad
import PrivateCloud.Provider.Types

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
        , faExpectedHash :: Hash
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

logConflict :: MonadIO m => EntryName -> Word64 -> Word64 -> m ()
logConflict file localSize cloudSize =
    liftIO $ noticeM syncLoggerName $ "#CONFLICT #file " ++ printEntry file
        ++ " #localsize " ++ show localSize ++ " #serversize " ++ show cloudSize

logLocalNew :: MonadIO m => EntryName -> Word64 -> m ()
logLocalNew file size =
    liftIO $ noticeM syncLoggerName $ "#NEW_LOCAL #file " ++ printEntry file ++ " #size " ++ show size

logLocalDelete :: MonadIO m => EntryName -> m ()
logLocalDelete file = liftIO $ noticeM syncLoggerName $ "#DEL_LOCAL #file " ++ printEntry file

logLocalChange :: MonadIO m => EntryName -> Word64 -> Word64 -> m ()
logLocalChange file oldSize newSize =
    liftIO $ noticeM syncLoggerName $ "#UPD_LOCAL #file " ++ printEntry file ++ " #size " ++ show newSize ++ " #oldsize " ++  show oldSize

logLocalMetadataChange :: MonadIO m => EntryName -> Timestamp -> Timestamp -> m ()
logLocalMetadataChange file oldTs newTs =
    liftIO $ noticeM syncLoggerName $ "#UPDMETA_LOCAL #file " ++ printEntry file
        ++ " #ts " ++ show newTs ++ " #oldts " ++  show oldTs

logServerNew :: MonadIO m => EntryName -> Word64 -> m ()
logServerNew file size =
    liftIO $ noticeM syncLoggerName $ "#NEW_SERVER #file " ++ printEntry file ++ " #size " ++ show size

logServerDelete :: MonadIO m => EntryName -> m ()
logServerDelete file = liftIO $ noticeM syncLoggerName $ "#DEL_SERVER #file " ++ printEntry file

logServerChange :: MonadIO m => EntryName -> DbFileInfo -> CloudFileInfo -> m ()
logServerChange file oldInfo newInfo =
    liftIO $ noticeM syncLoggerName $ "#UPD_SERVER #file " ++ printEntry file
        ++ " #size " ++ show (cfLength newInfo)
        ++ " #oldsize " ++  show (dfLength oldInfo)

logServerMetadataChange :: MonadIO m => EntryName -> Timestamp -> Timestamp -> m ()
logServerMetadataChange file oldTs newTs =
    liftIO $ noticeM syncLoggerName $ "#UPDMETA_SERVER #file " ++ printEntry file
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

filterGlob :: [Pattern] -> [(EntryName, a)] -> [(EntryName, a)]
filterGlob patterns = filter (\f -> not $ any (`match` entryFile (fst f)) patterns)

getAllFileChanges :: LocalFileList -> DbFileList -> CloudFileList -> PrivateCloud p [FileAction]
getAllFileChanges local db cloud = do
    root <- rootDir
    patterns <- exclusions
    let dropExclusions = filterGlob patterns
    getFileChanges False root (dropExclusions local) (dropExclusions db) (dropExclusions cloud)

getRecentFileChanges :: LocalFileList -> DbFileList -> CloudFileList -> PrivateCloud p [FileAction]
getRecentFileChanges local db cloud = do
    root <- rootDir
    patterns <- exclusions
    let dropExclusions = filterGlob patterns
    getFileChanges True root (dropExclusions local) (dropExclusions db) (dropExclusions cloud)

getFileChanges :: Bool -> FilePath -> LocalFileList -> DbFileList -> CloudFileList -> PrivateCloud p [FileAction]
getFileChanges onlyRecentServerFiles root localFiles dbFiles cloudFiles = do
    changes <- forM files $ \case
        (_, Nothing, Nothing, Nothing) -> do
            -- should never happen
            liftIO $ criticalM syncLoggerName $ "internal error in sync: two Nothings in zip " ++ show files
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
        (filename, Nothing, Just DbFileInfo{}, Nothing) | onlyRecentServerFiles -> do
            -- local delete and nobody updated db recently, so file must be there
            -- delete cloud file
            logLocalDelete filename
            return $ Just $ DeleteCloudFile filename
        (filename, Nothing, Just DbFileInfo{}, Nothing) -> do
            -- both deleted
            -- delete local db entry
            logLocalDelete filename
            return $ Just $ DeleteLocalFile filename
        (filename, Nothing, Just DbFileInfo{}, Just CloudDeleteMarker) -> do
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
                    liftIO $ noticeM syncLoggerName $ "#UPD_SERVER_DELETE_LOCAL #file " ++ printEntry filename ++ " #size " ++ show cfLength
                    return $ Just $ UpdateLocalFile filename cloudinfo
        (filename, Just localinfo@LocalFileInfo{..}, Just DbFileInfo{..}, Nothing) | onlyRecentServerFiles -> do
            -- file deleted on server or we have no server status
            -- check if it was modified locally
            fileChanged <- isLocalFileChangedFast filename lfLength dfLength lfModTime dfModTime dfHash
            if | fileChanged -> do
                    -- local file changed, upload to cloud in any case
                    logLocalChange filename dfLength lfLength
                    return $ Just $ UpdateCloudFile filename localinfo
               | lfModTime /= dfModTime -> do
                    -- metadata updated, was not recently updated on server
                    -- try to update metadata, it will fail if file deleted
                    logLocalMetadataChange filename dfModTime lfModTime
                    return $ Just $ UpdateCloudMetadata filename localinfo dfHash
               | otherwise -> return Nothing -- no change at all
        (filename, Just localinfo@LocalFileInfo{..}, Just DbFileInfo{..}, Nothing) -> do
                -- file deleted on server
                -- check if it was modified locally
            fileChanged <- isLocalFileChanged filename lfLength dfLength dfHash
            if fileChanged
                then do
                    -- local file changed, upload to cloud in any case
                    logLocalChange filename dfLength lfLength
                    return $ Just $ UpdateCloudFile filename localinfo
                else do
                    -- no local change, for sure absent on server, delete
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
            localFileUpdated <- isLocalFileChangedFast filename lfLength dfLength lfModTime dfModTime dfHash
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
                    if not localMetadataUpdated && not cloudMetadataUpdated
                        then return Nothing
                        else syncModTimes filename dfHash localinfo cloudinfo

    return $ catMaybes changes
  where
    files = zipLists3 localFiles dbFiles cloudFiles
    isLocalFileChangedFast filename newLength oldLength newTs oldTs oldHash
        | newLength /= oldLength = return True  -- length changed => file changed
        | newTs == oldTs = return False         -- length is the same and ts is the same => nothing changed
        | otherwise = isLocalFileContentChanged filename oldHash
    isLocalFileChanged filename newLength oldLength oldHash
        | newLength /= oldLength = return True
        | otherwise = isLocalFileContentChanged filename oldHash
    isLocalFileContentChanged filename oldHash = do
        liftIO $ infoM syncLoggerName $ "#CHECK_HASH #file " ++ printEntry filename
        localHash <- liftIO $ getFileHash (root </> entry2path filename)
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
