{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module PrivateCloud.Aws.SimpleDb where

import Aws.Aws
import Aws.Core (defServiceConfig)
import Aws.SimpleDb
import Conduit
import Control.Monad
import Data.Function
import Data.List
import Data.Maybe
import Data.Time.Clock.POSIX
import qualified Data.Text as T
import qualified Data.Text.Read as T

import PrivateCloud.Aws.Logging
import PrivateCloud.Aws.Monad
import PrivateCloud.Aws.Util
import PrivateCloud.Provider.Types

uploadFileInfo :: EntryName -> CloudFileInfo -> AwsMonad ()
uploadFileInfo (EntryName file) CloudFileInfo{..} = do
-- change to a single attribute with a record storing all values.
-- no need to bother if they present or not, also makes encryption easier.
-- XXX think about versioning? Maybe use protobufs or bond.
    sdbLogInfo $ "DB_WRITE_RECORD #file " ++ show file
    timestr <- printTime <$> liftIO getPOSIXTime
    domain <- acDomain <$> awsContext
    let command = putAttributes file
            [ replaceAttribute "hash" (hash2text cfHash)
            , replaceAttribute "size" (printSingle cfLength)
            , replaceAttribute "mtime" (printSingle cfModTime)
            , replaceAttribute "storage" (storageid2text cfStorageId)
            , replaceAttribute "recordmtime" timestr
            ]
            domain
    void $ awsReq command

uploadDeleteMarker :: EntryName -> AwsMonad ()
uploadDeleteMarker (EntryName file) = do
    sdbLogInfo $ "DB_MARK_DELETED #file " ++ show file
    timestr <- printTime <$> liftIO getPOSIXTime
    domain <- acDomain <$> awsContext
    let command = putAttributes file
            [ replaceAttribute "hash" T.empty
            , replaceAttribute "size" T.empty
            , replaceAttribute "mtime" T.empty
            , replaceAttribute "storage" T.empty
            , replaceAttribute "recordmtime" timestr
            ]
            domain
    void $ awsReq command

uploadFileMetadata :: EntryName -> DbFileInfo -> AwsMonad ()
uploadFileMetadata (EntryName file) DbFileInfo{..} = do
-- change to a single attribute with a record storing all values.
-- no need to bother if they present or not, also makes encryption easier.
-- XXX think about versioning? Maybe use protobufs or bond.
    sdbLogInfo $ "DB_UPDATE_RECORD #file " ++ show file
    timestr <- printTime <$> liftIO getPOSIXTime
    domain <- acDomain <$> awsContext
    let command = PutAttributes
            { paItemName = file
            , paAttributes =
                [ replaceAttribute "mtime" (printSingle dfModTime)
                , replaceAttribute "size" (printSingle dfLength)
                , replaceAttribute "recordmtime" timestr
                ]
            , paExpected = [ expectedValue "hash" (hash2text dfHash) ]
            , paDomainName = domain
            }
    void $ awsReq command

getAllServerFiles :: AwsMonad CloudFileList
getAllServerFiles = do
    sdbLogInfo "DB_GET_ALL_FILES_START"
    domain <- acDomain <$> awsContext
    files <- getServerFiles ("select * from `" <> domain <> "`")
    sdbLogInfo $ "DB_GET_ALL_FILES_END #files " ++ show (length files)
    pure files

-- get files updated in a last hour
getRecentServerFiles :: AwsMonad CloudFileList
getRecentServerFiles = do
    sdbLogInfo "DB_GET_RECENT_FILES_START"
    time <- liftIO getPOSIXTime
    domain <- acDomain <$> awsContext
    let recentTime = time - recentAge
    let timestr = printTime recentTime
    files <- getServerFiles $
        "select * from `" <> domain <> "` where recordmtime > '" <> timestr <> "'"
    sdbLogInfo $ "DB_GET_RECENT_FILES_END #files " ++ show (length files)
    pure files
  where
    recentAge :: POSIXTime
    recentAge = 3600

getServerFiles :: T.Text -> AwsMonad CloudFileList
getServerFiles queryText = do
    let query = (select queryText) { sConsistentRead = True }
    AwsContext{..} <- awsContext
    items <- liftIO $ runResourceT $ runConduit $
        awsIteratedList acConf defServiceConfig acManager query
            .| sinkList
    pure $ sortBy (compare `on` fst) $ mapMaybe conv items
  where
    -- FIXME report parse errors
    readDec t = case T.decimal t of
                    Right (v, "") -> Just v
                    _ -> Nothing
    getAttr name = fmap attributeData . find (\a -> attributeName a == name)
    conv Item{..} = do
        filehash <- getAttr "hash" itemData
        if T.null filehash
            then pure (EntryName itemName, CloudDeleteMarker)
            else do
                size <- readDec =<< getAttr "size" itemData
                mtime <- readDec =<< getAttr "mtime" itemData
                storage <- getAttr "storage" itemData
                pure
                    ( EntryName itemName
                    , CloudFile CloudFileInfo
                        { cfLength = size
                        , cfModTime = Timestamp mtime
                        , cfHash = Hash filehash
                        , cfStorageId = StorageId storage
                        }
                    )
