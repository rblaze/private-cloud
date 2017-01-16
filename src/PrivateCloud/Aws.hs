{-# Language BangPatterns, OverloadedStrings #-}
module PrivateCloud.Aws where

import Aws.Aws
import Aws.Core
import Aws.SimpleDb
import Control.Exception.Safe
import Control.Monad
import Crypto.Hash
import Crypto.MAC.HMAC
import Data.ByteArray
import Data.ByteArray.Encoding
import Network.HTTP.Client (Manager)
import System.FilePath
import System.Posix.Files
import System.Posix.IO
import System.Posix.Types
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
-- import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

domain :: T.Text
domain = "privatecloud"

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

uploadFileInfo :: Configuration -> Manager -> FilePath -> FilePath -> IO ()
uploadFileInfo config manager root file = bracket
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
                    domain
            void $ memoryAws config defServiceConfig manager command
            print file
            print fileHash
