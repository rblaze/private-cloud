module PrivateCloud.Cloud.Crypto where

import Crypto.MAC.HMAC
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import PrivateCloud.Provider.FileInfo

hmacKey :: BS.ByteString
hmacKey = BS.pack [102,111,111,98,97,114]

getFileHash :: FilePath -> IO Hash
getFileHash filename = do
    bytes <- BL.toChunks <$> BL.readFile filename
    let digest = finalize $ updates (initialize hmacKey) bytes
    return $ hmac2hash digest
