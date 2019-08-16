module Sodium.Error where

import Control.Exception
import Control.Monad (unless)
import Foreign.C.Types (CInt)

newtype SodiumException = SodiumException String

instance Exception SodiumException

instance Show SodiumException where
    showsPrec _ (SodiumException func) =
        showString "libsodium function " . showString func . showString " failed"

sodiumFail :: String -> IO a
sodiumFail = throwIO . SodiumException

sCheck :: String -> IO CInt -> IO ()
sCheck funcname f = do
    ret <- f
    unless (ret == 0) $ sodiumFail funcname
