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

uploadFileInfo :: CloudInfo -> FilePath -> CloudFileInfo -> IO ()
uploadFileInfo CloudInfo{..} file CloudFileInfo{..} = do
-- change to a single attribute with a record storing all values.
-- no need to bother if they present or not, also makes encryption easier.
-- XXX think about versioning? Maybe use protobufs or bond.
    time <- getPOSIXTime
    let timestr = printInt $ left 14 '0' (round time :: Word64)
    let command = putAttributes (T.pack file)
            [ replaceAttribute "hash" (T.decodeUtf8 cfHash)
            , replaceAttribute "size" (printInt cfLength)
            , replaceAttribute "mtime" (printInt (round cfModTime :: Word64))
            , replaceAttribute "version" (versionToText cfVersion)
            , replaceAttribute "recordmtime" timestr
            ]
            ciDomain
    void $ memoryAws ciConfig defServiceConfig ciManager command

uploadDeleteMarker :: CloudInfo -> FilePath -> IO ()
uploadDeleteMarker CloudInfo{..} file = do
    time <- getPOSIXTime
    let timestr = printInt $ left 14 '0' (round time :: Word64)
    let command = putAttributes (T.pack file)
            [ replaceAttribute "hash" T.empty
            , replaceAttribute "size" T.empty
            , replaceAttribute "mtime" T.empty
            , replaceAttribute "version" T.empty
            , replaceAttribute "recordmtime" timestr
            ]
            ciDomain
    void $ memoryAws ciConfig defServiceConfig ciManager command

uploadFileMetadata :: CloudInfo -> FilePath -> DbFileInfo -> IO ()
uploadFileMetadata CloudInfo{..} file DbFileInfo{..} = do
-- change to a single attribute with a record storing all values.
-- no need to bother if they present or not, also makes encryption easier.
-- XXX think about versioning? Maybe use protobufs or bond.
    let command = PutAttributes
            { paItemName = T.pack file
            , paAttributes = [ replaceAttribute "mtime" (T.pack $ show dfModTime) ]
            , paExpected =
                [ expectedValue "hash" (T.decodeUtf8 dfHash)
                , expectedValue "size" (T.pack $ show dfLength)
                ]
            , paDomainName = ciDomain
            }
    void $ memoryAws ciConfig defServiceConfig ciManager command

removeFileInfo :: CloudInfo -> FilePath -> IO ()
removeFileInfo CloudInfo{..} file =
    void $ memoryAws ciConfig defServiceConfig ciManager $
        deleteAttributes (T.pack file) [] ciDomain

getServerFiles :: CloudInfo -> IO [(FilePath, CloudFileStatus)]
getServerFiles config = getServerFiles' config ("select * from " <> ciDomain config)

-- get files updated in a last hour
getRecentServerFiles :: CloudInfo -> IO [(FilePath, CloudFileStatus)]
getRecentServerFiles config = do
    time <- getPOSIXTime
    let recentTime = time - recentAge
    let timestr = printInt $ left 14 '0' (round recentTime :: Word64)
    getServerFiles' config $
        "select * from " <> ciDomain config <>
        " where recordmtime > '" <> timestr <> "'"
    where
    recentAge :: POSIXTime
    recentAge = 3600


getServerFiles' :: CloudInfo -> T.Text -> IO [(FilePath, CloudFileStatus)]
getServerFiles' CloudInfo{..} queryText = do
    let query = (select queryText) { sConsistentRead = True }
    items <- runConduitRes $
        awsIteratedList ciConfig defServiceConfig ciManager query
            .| sinkList
    return $ sortBy (compare `on` fst) $ mapMaybe conv items
    where
    readDec t = case T.decimal t of
                    Right (v, "") -> Just v
                    _ -> Nothing
    getAttr name = fmap attributeData . find (\a -> attributeName a == name)
    conv Item{..} = do
        filehash <- getAttr "hash" itemData
        if T.null filehash
            then return ( T.unpack itemName, CloudDeleteMarker )
            else do
                size <- readDec =<< getAttr "size" itemData
                mtime <- readDec =<< getAttr "mtime" itemData
                version <- getAttr "version" itemData
                return
                    ( T.unpack itemName
                    , CloudFile CloudFileInfo
                        { cfLength = size
                        , cfModTime = realToFrac (mtime :: Word64)
                        , cfHash = T.encodeUtf8 filehash
                        , cfVersion = VersionId version
                        }
                    )

printInt :: Buildable t => t -> T.Text
printInt = TL.toStrict . format "{}" . Only
