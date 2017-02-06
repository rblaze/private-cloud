{-# Language OverloadedStrings, RecordWildCards #-}
module PrivateCloud.Aws.SimpleDb where

import Aws.Aws
import Aws.Core (defServiceConfig)
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
import System.Log.Logger
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Read as T

import PrivateCloud.Aws
import PrivateCloud.FileInfo

sdbLoggerName :: String
sdbLoggerName = "PrivateCloud.AWS.SimpleDb"

sdbLogInfo :: MonadIO m => String -> m ()
sdbLogInfo = liftIO . infoM sdbLoggerName

uploadFileInfo :: CloudInfo -> EntryName -> CloudFileInfo -> IO ()
uploadFileInfo CloudInfo{..} (EntryName file) CloudFileInfo{..} = do
-- change to a single attribute with a record storing all values.
-- no need to bother if they present or not, also makes encryption easier.
-- XXX think about versioning? Maybe use protobufs or bond.
    sdbLogInfo $ "#DB_UPLOAD #file " ++ show file
    timestr <- printTime <$> getPOSIXTime
    let command = putAttributes file
            [ replaceAttribute "hash" (hash2text cfHash)
            , replaceAttribute "size" (printSingle cfLength)
            , replaceAttribute "mtime" (printSingle cfModTime)
            , replaceAttribute "version" (version2text cfVersion)
            , replaceAttribute "recordmtime" timestr
            ]
            ciDomain
    void $ memoryAws ciConfig defServiceConfig ciManager command

uploadDeleteMarker :: CloudInfo -> EntryName -> IO ()
uploadDeleteMarker CloudInfo{..} (EntryName file) = do
    sdbLogInfo $ "#DB_MARK_DELETED #file " ++ show file
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
    sdbLogInfo $ "#DB_UPDATE #file " ++ show file
    timestr <- printTime <$> getPOSIXTime
    let command = PutAttributes
            { paItemName = file
            , paAttributes =
                [ replaceAttribute "mtime" (printSingle dfModTime)
                , replaceAttribute "size" (printSingle dfLength)
                , replaceAttribute "recordmtime" timestr
                ]
            , paExpected = [ expectedValue "hash" (hash2text dfHash) ]
            , paDomainName = ciDomain
            }
    void $ memoryAws ciConfig defServiceConfig ciManager command

removeFileInfo :: CloudInfo -> EntryName -> IO ()
removeFileInfo CloudInfo{..} (EntryName file) = do
    sdbLogInfo $ "#DB_DELETE #file " ++ show file
    void $ memoryAws ciConfig defServiceConfig ciManager $
        deleteAttributes file [] ciDomain

getAllServerFiles :: CloudInfo -> IO CloudFileList
getAllServerFiles config = do
    sdbLogInfo "#DB_GET_ALL"
    getServerFiles config ("select * from " <> ciDomain config)

-- get files updated in a last hour
getRecentServerFiles :: CloudInfo -> IO CloudFileList
getRecentServerFiles config = do
    sdbLogInfo "#DB_GET_RECENT"
    time <- getPOSIXTime
    let recentTime = time - recentAge
    let timestr = printTime recentTime
    getServerFiles config $
        "select * from " <> ciDomain config <>
        " where recordmtime > '" <> timestr <> "'"
    where
    recentAge :: POSIXTime
    recentAge = 3600

getServerFiles :: CloudInfo -> T.Text -> IO CloudFileList
getServerFiles CloudInfo{..} queryText = do
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
                        , cfModTime = Timestamp mtime
                        , cfHash = Hash filehash
                        , cfVersion = VersionId version
                        }
                    )

printSingle :: Buildable t => t -> T.Text
printSingle = TL.toStrict . format "{}" . Only

printTime :: POSIXTime -> T.Text
printTime time = printSingle $ left 14 '0' (round time :: Word64)
