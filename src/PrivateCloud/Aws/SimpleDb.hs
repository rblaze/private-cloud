{-# Language OverloadedStrings, RecordWildCards #-}
module PrivateCloud.Aws.SimpleDb where

import Aws.Aws
import Aws.Core
import Aws.SimpleDb
import Conduit
import Control.Monad
import Data.Function
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Text.Buildable
import Data.Text.Format
import Data.Time.Clock.POSIX
import Data.Word
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as T
import qualified Data.Text.Read as T

import PrivateCloud.Aws
import PrivateCloud.FileInfo

uploadFileInfo :: CloudInfo -> EntryName -> CloudFileInfo -> IO ()
uploadFileInfo CloudInfo{..} (EntryName file) CloudFileInfo{..} = do
-- change to a single attribute with a record storing all values.
-- no need to bother if they present or not, also makes encryption easier.
-- XXX think about versioning? Maybe use protobufs or bond.
    timestr <- printTime <$> getPOSIXTime
    let command = putAttributes file
            [ replaceAttribute "hash" (T.decodeUtf8 cfHash)
            , replaceAttribute "size" (printInt cfLength)
            , replaceAttribute "mtime" (printInt (round cfModTime :: Word64))
            , replaceAttribute "version" (versionToText cfVersion)
            , replaceAttribute "recordmtime" timestr
            ]
            ciDomain
    void $ memoryAws ciConfig defServiceConfig ciManager command

uploadDeleteMarker :: CloudInfo -> EntryName -> IO ()
uploadDeleteMarker CloudInfo{..} (EntryName file) = do
    timestr <- printTime <$> getPOSIXTime
    let command = putAttributes file
            [ replaceAttribute "hash" T.empty
            , replaceAttribute "size" T.empty
            , replaceAttribute "mtime" T.empty
            , replaceAttribute "version" T.empty
            , replaceAttribute "recordmtime" timestr
            ]
            ciDomain
    void $ memoryAws ciConfig defServiceConfig ciManager command

uploadFileMetadata :: CloudInfo -> EntryName -> DbFileInfo -> IO ()
uploadFileMetadata CloudInfo{..} (EntryName file) DbFileInfo{..} = do
-- change to a single attribute with a record storing all values.
-- no need to bother if they present or not, also makes encryption easier.
-- XXX think about versioning? Maybe use protobufs or bond.
    let command = PutAttributes
            { paItemName = file
            , paAttributes =
                [ replaceAttribute "mtime" (printInt (round dfModTime :: Word64))
                , replaceAttribute "size" (T.pack $ show dfLength)
                ]
            , paExpected = [ expectedValue "hash" (T.decodeUtf8 dfHash) ]
            , paDomainName = ciDomain
            }
    void $ memoryAws ciConfig defServiceConfig ciManager command

removeFileInfo :: CloudInfo -> EntryName -> IO ()
removeFileInfo CloudInfo{..} (EntryName file) =
    void $ memoryAws ciConfig defServiceConfig ciManager $
        deleteAttributes file [] ciDomain

getServerFiles :: CloudInfo -> IO CloudFileList
getServerFiles config = getServerFiles' config ("select * from " <> ciDomain config)

-- get files updated in a last hour
getRecentServerFiles :: CloudInfo -> IO CloudFileList
getRecentServerFiles config = do
    time <- getPOSIXTime
    let recentTime = time - recentAge
    let timestr = printTime recentTime
    getServerFiles' config $
        "select * from " <> ciDomain config <>
        " where recordmtime > '" <> timestr <> "'"
    where
    recentAge :: POSIXTime
    recentAge = 3600


getServerFiles' :: CloudInfo -> T.Text -> IO CloudFileList
getServerFiles' CloudInfo{..} queryText = do
    let query = (select queryText) { sConsistentRead = True }
    items <- runConduitRes $
        awsIteratedList ciConfig defServiceConfig ciManager query
            .| sinkList
    return $ sortBy (compare `on` fst) $ mapMaybe conv items
    where
    -- FIXME report parse errors
    readDec t = case T.decimal t of
                    Right (v, "") -> Just v
                    _ -> Nothing
    getAttr name = fmap attributeData . find (\a -> attributeName a == name)
    conv Item{..} = do
        filehash <- getAttr "hash" itemData
        if T.null filehash
            then return ( EntryName itemName, CloudDeleteMarker )
            else do
                size <- readDec =<< getAttr "size" itemData
                mtime <- readDec =<< getAttr "mtime" itemData
                version <- getAttr "version" itemData
                return
                    ( EntryName itemName
                    , CloudFile CloudFileInfo
                        { cfLength = size
                        , cfModTime = realToFrac (mtime :: Word64)
                        , cfHash = T.encodeUtf8 filehash
                        , cfVersion = VersionId version
                        }
                    )

printInt :: Buildable t => t -> T.Text
printInt = TL.toStrict . format "{}" . Only

printTime :: POSIXTime -> T.Text
printTime time = printInt $ left 14 '0' (round time :: Word64)
