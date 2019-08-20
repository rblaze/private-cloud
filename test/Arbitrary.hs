module Arbitrary (ArbByteString(..)) where

import Test.QuickCheck (Arbitrary(..), resize)
import Test.QuickCheck.Instances.ByteString()
import qualified Data.ByteString as BS

newtype ArbByteString = ABS { fromABS :: BS.ByteString }
    deriving Show

instance Arbitrary ArbByteString where
    arbitrary = ABS <$> resize (100 * 1024) arbitrary
    shrink (ABS bs) = map ABS $ shrink bs
