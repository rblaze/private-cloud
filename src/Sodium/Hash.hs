module Sodium.Hash where

import Sodium.Error
import Sodium.FFI

import Data.ByteArray as BA
import Foreign.Ptr (nullPtr)
import qualified Data.ByteString as BS

hashSimple :: ByteArrayAccess b => b -> IO BS.ByteString
hashSimple bytes = withByteArray bytes $ \iptr ->
     fmap snd $ allocRet generichash_BYTES $ \optr ->
        sCheck "crypto_generichash" $ pure $
            c_generichash
                optr generichash_BYTES
                iptr (fromIntegral $ BA.length bytes)
                nullPtr 0

newtype HashContext = HashContext Bytes

hashInit :: IO HashContext
hashInit = do
    let ctxsize = fromIntegral c_generichash_statebytes
    fmap (HashContext . snd) $ allocRet ctxsize $ \ptr ->
        sCheck "crypto_generichash_init" $
            c_generichash_init ptr nullPtr 0 generichash_BYTES

hashUpdate :: ByteArrayAccess b => HashContext -> b -> IO ()
hashUpdate (HashContext ctx) b = withByteArray ctx $ \ctxptr ->
    withByteArray b $ \bptr ->
        sCheck "crypto_generichash_update" $
            c_generichash_update ctxptr bptr (fromIntegral $ BA.length b)

hashFinal :: HashContext -> IO BS.ByteString
hashFinal (HashContext ctx) = withByteArray ctx $ \ctxptr ->
    fmap snd $ allocRet generichash_BYTES $ \optr ->
        sCheck "crypto_generichash_final" $
            c_generichash_final ctxptr optr generichash_BYTES
