module Sodium.Init where

import Sodium.Error
import Sodium.FFI

sodiumInit :: IO ()
sodiumInit = sCheck "sodium_init" c_init
