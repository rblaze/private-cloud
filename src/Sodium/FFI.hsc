{-# LANGUAGE ForeignFunctionInterface #-}
module Sodium.FFI where

import Foreign.C.Types
import Foreign.Ptr

#include <sodium.h>

foreign import ccall "sodium_init" c_init :: IO CInt

generichash_KEYBYTES :: Word
generichash_KEYBYTES = #const crypto_generichash_KEYBYTES

generichash_KEYBYTES_MIN :: Word
generichash_KEYBYTES_MIN = #const crypto_generichash_KEYBYTES_MIN

generichash_KEYBYTES_MAX :: Word
generichash_KEYBYTES_MAX = #const crypto_generichash_KEYBYTES_MAX

generichash_BYTES :: Num a => a
generichash_BYTES = #const crypto_generichash_BYTES

generichash_BYTES_MIN :: Word
generichash_BYTES_MIN = #const crypto_generichash_BYTES_MIN

generichash_BYTES_MAX :: Word
generichash_BYTES_MAX = #const crypto_generichash_BYTES_MAX

data CRYPTO_generichash_state

-- This function is pure by design.
foreign import ccall "crypto_generichash" c_generichash :: Ptr CUChar -> CSize -> Ptr CUChar -> CULLong -> Ptr CUChar -> CSize -> CInt

foreign import ccall "crypto_generichash_keygen" c_generichash_keygen :: Ptr CUChar -> IO ()

foreign import ccall unsafe "crypto_generichash_statebytes" c_generichash_statebytes :: CSize

foreign import ccall "crypto_generichash_init" c_generichash_init :: Ptr CRYPTO_generichash_state -> Ptr CUChar -> CSize -> CSize -> IO CInt

foreign import ccall "crypto_generichash_update" c_generichash_update :: Ptr CRYPTO_generichash_state -> Ptr CUChar -> CULLong -> IO CInt

foreign import ccall "crypto_generichash_final" c_generichash_final :: Ptr CRYPTO_generichash_state -> Ptr CUChar -> CSize -> IO CInt
