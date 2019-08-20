module Sodium.Memory (SodiumBytes) where

import Sodium.Error
import Sodium.FFI

import Control.Monad (foldM_, when, forM_, void)
import Data.ByteArray
import Data.List.NonEmpty (NonEmpty(..))
import Data.Semigroup (Semigroup(..))
import Data.Void
import Foreign.ForeignPtr
import Foreign.Ptr
import System.IO.Unsafe (unsafePerformIO, unsafeDupablePerformIO)
import qualified Data.List.NonEmpty as NE

data SodiumBytes = SodiumBytes
    { sbLength :: !Int
    , sbPtr :: !(ForeignPtr Void)
    }

allocSB :: Int -> IO (ForeignPtr Void)
allocSB len = do
    ptr <- c_malloc (fromIntegral len)
    when (ptr == nullPtr) $ sodiumFail "sodium_malloc"
    newForeignPtr c_free_ptr ptr

instance ByteArrayAccess SodiumBytes where
    length = sbLength
    withByteArray ba act =
        withForeignPtr (sbPtr ba) $ act . castPtr

instance Eq SodiumBytes where
    (SodiumBytes l1 ptr1) == (SodiumBytes l2 ptr2)
      | l1 /= l2 = False
      | otherwise = unsafeDupablePerformIO $
            withForeignPtr ptr1 $ \p1 ->
                withForeignPtr ptr2 $ \p2 ->
                    pure $ c_memcmp p1 p2 (fromIntegral l1) == 0

instance Ord SodiumBytes where
    compare (SodiumBytes l1 ptr1) (SodiumBytes l2 ptr2)
        | cmp < 0 = LT
        | cmp > 0 = GT
        | otherwise = compare l1 l2
      where
        len = min l1 l2
        cmp = unsafeDupablePerformIO $
            withForeignPtr ptr1 $ \p1 ->
                withForeignPtr ptr2 $ \p2 ->
                    pure $ std_memcmp p1 p2 (fromIntegral len)

instance Semigroup SodiumBytes where
    (SodiumBytes l1 ptr1) <> (SodiumBytes l2 ptr2) = unsafePerformIO $ do
        dest <- allocSB (l1 + l2)
        withForeignPtr dest $ \pdest -> do
            withForeignPtr ptr1 $ \p1 ->
                void $ std_memcpy pdest p1 (fromIntegral l1)
            withForeignPtr ptr2 $ \p2 ->
                void $ std_memcpy (pdest `plusPtr` l1) p2 (fromIntegral l2)
        pure $ SodiumBytes (l1 + l2) dest

    sconcat ptrs = unsafePerformIO $ do
        let totalLen = sum $ NE.map sbLength ptrs
        dest <- allocSB totalLen
        withForeignPtr dest $ \pdest ->
            foldM_ (copyBytes pdest) 0 ptrs
        pure $ SodiumBytes totalLen dest
      where
        copyBytes pdest offset src =
            withForeignPtr (sbPtr src) $ \psrc -> do
                void $ std_memcpy (pdest `plusPtr` offset) psrc (fromIntegral $ sbLength src)
                pure $ offset + sbLength src

    stimes n ptr = unsafePerformIO $ do
        let intN = fromIntegral n
        let blockSize = sbLength ptr
        let totalLen = intN * blockSize
        dest <- allocSB totalLen
        withForeignPtr dest $ \pdest ->
            withForeignPtr (sbPtr ptr) $ \psrc ->
                forM_ [0 .. intN - 1] $ \i ->
                    std_memcpy (pdest `plusPtr` (i * blockSize)) psrc (fromIntegral blockSize)
        pure $ SodiumBytes totalLen dest

instance Monoid SodiumBytes where
    mempty = unsafePerformIO $ do
        ptr <- allocSB 0
        pure $ SodiumBytes 0 ptr

    mconcat [] = mempty
    mconcat (x:xs) = sconcat (x :| xs)

instance ByteArray SodiumBytes where
    allocRet len act = do
        ptr <- allocSB len
        v <- withForeignPtr ptr $ act . castPtr
        pure (v, SodiumBytes len ptr)
