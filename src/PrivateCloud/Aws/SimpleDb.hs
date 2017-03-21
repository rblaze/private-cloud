{-# Language OverloadedStrings, RecordWildCards #-}
module PrivateCloud.Aws.SimpleDb where

import Aws.Aws
import Aws.Core (defServiceConfig)
import Aws.SimpleDb
import Conduit
import Control.Monad
import Control.Monad.Trans.Resource
import Data.Function
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Text.Buildable
import Data.Text.Format
import Data.Time.Clock.POSIX
import Data.Word
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS
import System.Log.Logger
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Read as T

import PrivateCloud.Aws.Monad
import PrivateCloud.Provider.Types

sdbLoggerName :: String
sdbLoggerName = "PrivateCloud.AWS.SimpleDb"

sdbLogInfo :: MonadIO m => String -> m ()
sdbLogInfo = liftIO . infoM sdbLoggerName

uploadFileInfo :: EntryName -> CloudFileInfo -> AwsMonad ()
uploadFileInfo (EntryName file) CloudFileInfo{..} = do
-- change to a single attribute with a record storing all values.
-- no need to bother if they present or not, also makes encryption easier.
-- XXX think about versioning? Maybe use protobufs or bond.
    sdbLogInfo $ "#DB_UPLOAD #file " ++ show file
    timestr <- printTime <$> liftIO getPOSIXTime
    domain <- awsAsks acDomain
    let command = putAttributes file
            [ replaceAttribute "hash" (hash2text cfHash)
            , replaceAttribute "size" (printSingle cfLength)
            , replaceAttribute "mtime" (printSingle cfModTime)
            , replaceAttribute "storage" (storage2text cfStorageId)
            , replaceAttribute "recordmtime" timestr
            ]
            domain
    conf <- awsAsks acLegacyConf
    void $ simpleAws conf defServiceConfig command

uploadDeleteMarker :: EntryName -> AwsMonad ()
uploadDeleteMarker (EntryName file) = do
    sdbLogInfo $ "#DB_MARK_DELETED #file " ++ show file
    timestr <- printTime <$> liftIO getPOSIXTime
    domain <- awsAsks acDomain
    let command = putAttributes file
            [ replaceAttribute "hash" T.empty
            , replaceAttribute "size" T.empty
            , replaceAttribute "mtime" T.empty
            , replaceAttribute "storage" T.empty
            , replaceAttribute "recordmtime" timestr
            ]
            domain
    conf <- awsAsks acLegacyConf
    void $ simpleAws conf defServiceConfig command

uploadFileMetadata :: EntryName -> DbFileInfo -> AwsMonad ()
uploadFileMetadata (EntryName file) DbFileInfo{..} = do
-- change to a single attribute with a record storing all values.
-- no need to bother if they present or not, also makes encryption easier.
-- XXX think about versioning? Maybe use protobufs or bond.
    sdbLogInfo $ "#DB_UPDATE #file " ++ show file
    timestr <- printTime <$> liftIO getPOSIXTime
    domain <- awsAsks acDomain
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
    conf <- awsAsks acLegacyConf
    void $ simpleAws conf defServiceConfig command

removeFileInfo :: EntryName -> AwsMonad ()
removeFileInfo (EntryName file) = do
    sdbLogInfo $ "#DB_DELETE #file " ++ show file
    domain <- awsAsks acDomain
    conf <- awsAsks acLegacyConf
    void $ simpleAws conf defServiceConfig $
        deleteAttributes file [] domain

getAllServerFiles :: AwsMonad CloudFileList
getAllServerFiles = do
    sdbLogInfo "#DB_GET_ALL"
    domain <- awsAsks acDomain
    files <- getServerFiles ("select * from `" <> domain <> "`")
    sdbLogInfo $ "#DB_GET_ALL_RESULT #files " ++ show (length files)
    return files

-- get files updated in a last hour
getRecentServerFiles :: AwsMonad CloudFileList
getRecentServerFiles = do
    sdbLogInfo "#DB_GET_RECENT"
    time <- liftIO getPOSIXTime
    domain <- awsAsks acDomain
    let recentTime = time - recentAge
    let timestr = printTime recentTime
    files <- getServerFiles $
        "select * from `" <> domain <> "` where recordmtime > '" <> timestr <> "'"
    sdbLogInfo $ "#DB_GET_RECENT_RESULT #files " ++ show (length files)
    return files
    where
    recentAge :: POSIXTime
    recentAge = 3600

getServerFiles :: T.Text -> AwsMonad CloudFileList
getServerFiles queryText = do
    let query = (select queryText) { sConsistentRead = True }
    conf <- awsAsks acLegacyConf
    manager <- liftIO $ newManager tlsManagerSettings
    items <- liftResourceT $ runConduit $
        awsIteratedList conf defServiceConfig manager query
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
            then return (EntryName itemName, CloudDeleteMarker)
            else do
                size <- readDec =<< getAttr "size" itemData
                mtime <- readDec =<< getAttr "mtime" itemData
                storage <- getAttr "storage" itemData
                return
                    ( EntryName itemName
                    , CloudFile CloudFileInfo
                        { cfLength = size
                        , cfModTime = Timestamp mtime
                        , cfHash = Hash filehash
                        , cfStorageId = StorageId storage
                        }
                    )

printSingle :: Buildable t => t -> T.Text
printSingle = TL.toStrict . format "{}" . Only

printTime :: POSIXTime -> T.Text
printTime time = printSingle $ left 14 '0' (round time :: Word64)
