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

uploadFileInfo :: CloudInfo -> FilePath -> FileInfo -> IO ()
uploadFileInfo CloudInfo{..} file FileInfo{..} = do
-- change to a single attribute with a record storing all values.
-- no need to bother if they present or not, also makes encryption easier.
-- XXX think about versioning? Maybe use protobufs or bond.
    let command = putAttributes (T.pack file)
            [ replaceAttribute "hash" (T.decodeUtf8 fiHash)
            , replaceAttribute "size" (T.pack $ show fiLength)
            , replaceAttribute "mtime" (T.pack $ show fiModTime)
            ]
            ciDomain
    void $ memoryAws ciConfig defServiceConfig ciManager command

removeFileInfo :: CloudInfo -> FilePath -> IO ()
removeFileInfo CloudInfo{..} file =
    void $ memoryAws ciConfig defServiceConfig ciManager $
        deleteAttributes (T.pack file) [] ciDomain

getServerFiles :: CloudInfo -> IO [(FilePath, FileInfo)]
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
        return
            ( T.unpack itemName
            , FileInfo
                { fiLength = size
                , fiModTime = CTime mtime
                , fiHash = T.encodeUtf8 filehash
                }
            )
