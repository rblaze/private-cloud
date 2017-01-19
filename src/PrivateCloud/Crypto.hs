{-# Language BangPatterns #-}
module PrivateCloud.Crypto where

import Data.ByteArray.Encoding
import Crypto.Hash
import Crypto.MAC.HMAC
import Data.ByteArray
import System.Posix.IO
import System.Posix.Types
import qualified Data.ByteString as BS

hmacKey :: BS.ByteString
hmacKey = BS.pack [102,111,111,98,97,114]

getFileHash :: Fd -> IO BS.ByteString
getFileHash fd = do
    let bufsize = 1024 * 1024
    array <- create bufsize $ const $ return ()
    let loop !ctx = do
            bytesRead <- withByteArray (array :: Bytes) $ \ptr ->
                fdReadBuf fd ptr (fromIntegral bufsize)
            if bytesRead == 0
                then return $ finalize ctx
                else loop $ update ctx (takeView array $ fromIntegral bytesRead)
    let ctx = initialize hmacKey
    result <- loop ctx
    let digest = hmacGetDigest result :: Digest SHA512t_256
    return $ convertToBase Base64 digest
