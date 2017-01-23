{-# Language BangPatterns #-}
module PrivateCloud.Crypto where

import Data.ByteArray.Encoding
import Crypto.Hash
import Crypto.MAC.HMAC
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

hmacKey :: BS.ByteString
hmacKey = BS.pack [102,111,111,98,97,114]

getFileHash :: FilePath -> IO BS.ByteString
getFileHash filename = do
    bytes <- BL.toChunks <$> BL.readFile filename
    let digest = finalize $ updates (initialize hmacKey) bytes
    return $ convertToBase Base64 (digest :: HMAC SHA512t_256)
