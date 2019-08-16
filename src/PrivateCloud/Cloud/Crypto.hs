module PrivateCloud.Cloud.Crypto where

import qualified Data.ByteString.Lazy as BL

import PrivateCloud.Provider.Types
import Sodium.Hash

makeFileHash :: FilePath -> IO Hash
makeFileHash filename = do
    bytes <- BL.readFile filename
    ctx <- hashInit
    mapM_ (hashUpdate ctx) $ BL.toChunks bytes
    encodeHash <$> hashFinal ctx
