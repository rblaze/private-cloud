{-# Language BangPatterns, OverloadedStrings, RecordWildCards #-}
module PrivateCloud.Aws where

import Aws.Aws
import Aws.Core
import Aws.SimpleDb
import Control.Exception.Safe
import Control.Monad
import Control.Monad.Trans.Resource
import Crypto.Hash
import Crypto.MAC.HMAC
import Data.ByteArray hiding (concat)
import Data.ByteArray.Encoding
import Data.List
import Data.Maybe
import Data.Monoid
import Foreign.C.Types
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS
import System.FilePath
import System.Posix.Files
import System.Posix.IO
import System.Posix.Types
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
-- import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Read as T

import PrivateCloud.DirTree

data CloudInfo = CloudInfo
    { ciConfig :: Configuration
    , ciManager :: Manager
    , ciDomain :: T.Text
    }

defaultCloudInfo :: IO CloudInfo
defaultCloudInfo = do
    manager <- newManager tlsManagerSettings
    config <- baseConfiguration
    return CloudInfo
        { ciConfig = config
        , ciManager = manager
        , ciDomain = "privatecloud"
        }

getFileHash :: Fd -> IO (Digest SHA512t_256)
getFileHash fd = do
    let bufsize = 1024 * 1024
    array <- create bufsize $ const $ return ()
    let loop !ctx = do
            bytesRead <- withByteArray (array :: Bytes) $ \ptr ->
                fdReadBuf fd ptr (fromIntegral bufsize)
            if bytesRead == 0
                then return $ finalize ctx
                else loop $ update ctx (takeView array $ fromIntegral bytesRead)
    let ctx = initialize $ BS.pack [102,111,111,98,97,114]
    result <- loop ctx
    return $ hmacGetDigest result

uploadFileInfo :: CloudInfo -> FilePath -> FilePath -> IO ()
uploadFileInfo CloudInfo{..} root file = bracket
    (openFd (root </> file) ReadOnly Nothing defaultFileFlags)
    closeFd
    $ \fd -> do
        status <- getFdStatus fd
        when (isRegularFile status) $ do
            let size = fileSize status
            let mtime = modificationTime status
            fileHash <- getFileHash fd
            let hashVal = convertToBase Base64 fileHash
            let command = putAttributes (T.pack file)
                    [ replaceAttribute "hash" (T.pack $ BS8.unpack hashVal)
                    , replaceAttribute "size" (T.pack $ show size)
                    , replaceAttribute "mtime" (T.pack $ show mtime)
                    ]
                    ciDomain
            void $ memoryAws ciConfig defServiceConfig ciManager command
            print file
            print fileHash

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
    return $ mapMaybe conv $ concat items
    where
    readDec t = case T.decimal t of
                    Right (v, "") -> Just v
                    _ -> Nothing
    conv Item{..} = do
        let filehash = attributeData <$> find (\a -> attributeName a == "hash") itemData
        size <- readDec =<< attributeData <$> find (\a -> attributeName a == "size") itemData
        mtime <- readDec =<< attributeData <$> find (\a -> attributeName a == "mtime") itemData
        return
            ( T.unpack itemName
            , FileInfo
                { fiLength = size
                , fiModTime = CTime mtime
                , fiHash = filehash
                }
            )
