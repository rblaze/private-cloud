{-# Language OverloadedStrings, RecordWildCards #-}
module PrivateCloud.Aws.SimpleDb where

import Aws.Aws
import Aws.Core
import Aws.SimpleDb
import Control.Monad
import Control.Monad.Trans.Resource
import Data.Function
import Data.List
import Data.Maybe
import Data.Monoid
import Foreign.C.Types
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Read as T

import PrivateCloud.Aws
import PrivateCloud.FileInfo

uploadFileInfo :: CloudInfo -> FilePath -> CloudFileInfo -> IO ()
uploadFileInfo CloudInfo{..} file CloudFileInfo{..} = do
-- change to a single attribute with a record storing all values.
-- no need to bother if they present or not, also makes encryption easier.
-- XXX think about versioning? Maybe use protobufs or bond.
    let command = putAttributes (T.pack file)
            [ replaceAttribute "hash" (T.decodeUtf8 cfHash)
            , replaceAttribute "size" (T.pack $ show cfLength)
            , replaceAttribute "mtime" (T.pack $ show cfModTime)
            , replaceAttribute "version" (versionToText cfVersion)
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

getServerFiles :: CloudInfo -> IO [(FilePath, CloudFileInfo)]
getServerFiles CloudInfo{..} = do
    let svcConfig = defServiceConfig
    let loop Nothing = return []
        loop (Just req) = do
            (segment, nextReq) <- runResourceT $ do
                resp <- pureAws ciConfig svcConfig ciManager req
                let segment = listResponse resp
                let nextReq = nextIteratedRequest req resp
                return (segment, nextReq)
            rest <- loop nextReq
            return $ segment : rest
    let firstQuery = Select
            { sSelectExpression = "select * from " <> ciDomain
            , sConsistentRead = True
            , sNextToken = Nothing
            }
    items <- loop $ Just firstQuery
    return $ sortBy (compare `on` fst) $ mapMaybe conv $ concat items
    where
    readDec t = case T.decimal t of
                    Right (v, "") -> Just v
                    _ -> Nothing
    getAttr name = fmap attributeData . find (\a -> attributeName a == name)
    conv Item{..} = do
        filehash <- getAttr "hash" itemData
        size <- readDec =<< getAttr "size" itemData
        mtime <- readDec =<< getAttr "mtime" itemData
        version <- getAttr "version" itemData
        return
            ( T.unpack itemName
            , CloudFileInfo
                { cfLength = size
                , cfModTime = CTime mtime
                , cfHash = T.encodeUtf8 filehash
                , cfVersion = ObjectVersion version
                }
            )
