{-# LANGUAGE OverloadedStrings #-}
module SodiumTests (sodiumTests) where

import Control.Monad (forM, forM_, when, unless)
import Data.ByteArray as BA
import Data.ByteArray.Encoding
import Data.Word (Word8)
import Data.Semigroup (stimes)
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe (unsafeDupablePerformIO)
import Test.QuickCheck.Monadic as Q
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.ByteString.Lazy as BL

import Arbitrary
import Sodium.FFI (std_memcmp)
import Sodium.Hash
import Sodium.Memory

sodiumTests :: TestTree
sodiumTests = testGroup "Sodium tests"
    [ testCase "simple hash" testHash
    , testCase "stream hash" testStreamHash
    , testCase "empty memory alloc" testEmptyMemoryAlloc
    , testProperty "memory alloc" testMemoryAlloc
    , testProperty "memory copy" testMemoryCopy
    , testCase "memory equality" testMemoryEq
    , testProperty "memory ordering" testMemoryOrd
    , testProperty "memory append" testMemoryAppend
    , testProperty "memory concat" testMemoryConcat
    , testProperty "memory stimes" testMemoryStimes
    ]

memEq :: BS.ByteString -> SodiumBytes -> Bool
memEq str buf
    | BS.length str /= BA.length buf = False
    | otherwise = unsafeDupablePerformIO $
        withByteArray buf $ \bufptr ->
            BS.unsafeUseAsCString str $ \strptr -> do
                let ret = std_memcmp bufptr (castPtr strptr) (fromIntegral $ BS.length str)
                pure $ ret == 0

testHash :: Assertion
testHash = do
    let src = BS.unfoldr
            (\i -> if i < 9876543 then Just (fromIntegral i, i + 1) else Nothing)
            (42 :: Int)
    hash <- hashSimple src
    let hex = convertToBase Base16 hash
    assertEqual "hash mismatch" "8665019f9bc50eaf32f020c89c03564ffd8ac47a180a1079e07b43a6ab1abe35" (hex :: BS.ByteString)

testStreamHash :: Assertion
testStreamHash = do
    let src = BL.unfoldr
            (\i -> if i < 9876543 then Just (fromIntegral i, i + 1) else Nothing)
            (42 :: Int)
    ctx <- hashInit
    mapM_ (hashUpdate ctx) $ BL.toChunks src
    hash <- hashFinal ctx
    let hex = convertToBase Base16 hash
    assertEqual "hash mismatch" "8665019f9bc50eaf32f020c89c03564ffd8ac47a180a1079e07b43a6ab1abe35" (hex :: BS.ByteString)

testMemoryAlloc :: ArbByteString -> Property
testMemoryAlloc (ABS golden) = monadicIO $ do
    let bufSize = BS.length golden
    ((), buf) <- run $ allocRet bufSize $ \ptr ->
        forM_ [0 .. bufSize - 1] $ \i ->
            pokeElemOff (ptr :: Ptr Word8) i (BS.index golden i)

    Q.assert $ bufSize == BA.length buf

    bs1 <- run $ withByteArray (buf :: SodiumBytes) $ \ptr ->
        BS.packCStringLen (ptr, BA.length buf)

    Q.assert $ golden == bs1

    bs2 <- run $ BA.copy buf $ const $ pure ()
    Q.assert $ golden == bs2

testMemoryCopy :: ArbByteString -> Property
testMemoryCopy (ABS bs) = monadicIO $ do
    buf <- run $ BA.copy bs $ const $ pure ()
    Q.assert $ memEq bs buf

testMemoryOrd :: ArbByteString -> ArbByteString -> Property
testMemoryOrd (ABS left) (ABS right) = monadicIO $ do
    bufLeft <- run $ BA.copy left $ const $ pure ()
    bufRight <- run $ BA.copy right $ const $ pure ()

    Q.assert $ compare left right == compare (bufLeft :: SodiumBytes) bufRight

testEmptyMemoryAlloc :: Assertion
testEmptyMemoryAlloc = do
    buf1 <- BA.alloc 0 $ const $ pure ()
    let buf2 = mempty :: SodiumBytes
    assertEqual "invalid empty bytearray length" 0 (BA.length buf1)
    assertEqual "invalid mempty bytearray length" 0 (BA.length buf2)

    bs <- BA.copy (buf1 :: SodiumBytes) $ const $ pure ()
    assertEqual "copied empty value mismatch" BS.empty bs

testMemoryEq :: Assertion
testMemoryEq = do
    empty1 <- BA.alloc 0 $ const $ pure ()
    empty2 <- BA.alloc 0 $ const $ pure ()
    unless (empty1 == (empty2 :: SodiumBytes)) $
        assertFailure "empty bytearrays not equal"

    buf1 <- BA.copy ("foobar" :: BS.ByteString) $ const $ pure ()
    buf2 <- BA.copy ("foobar" :: BS.ByteString) $ const $ pure ()
    buf3 <- BA.copy ("barfoo" :: BS.ByteString) $ const $ pure ()
    buf4 <- BA.copy ("foobar1" :: BS.ByteString) $ const $ pure ()
    when (buf1 == empty1) $
        assertFailure "empty bytearray equal to non-empty"
    unless (buf1 == buf2) $
        assertFailure "identical bytearrays not equal"
    when (buf1 == buf3) $
        assertFailure "bytearrays with different content are equal"
    when (buf1 == buf4) $
        assertFailure "bytearrays with same prefix are equal"

testMemoryAppend :: ArbByteString -> ArbByteString -> Property
testMemoryAppend (ABS left) (ABS right) = monadicIO $ do
    bufLeft <- run $ BA.copy left $ const $ pure ()
    bufRight <- run $ BA.copy right $ const $ pure ()

    let buf = bufLeft <> bufRight
    let str = left <> right
    Q.assert $ memEq str buf

testMemoryConcat :: [ArbByteString] -> Property
testMemoryConcat bss = monadicIO $ do
    bufs <- run $ forM bss $ \(ABS bs) -> BA.copy bs $ const $ pure ()

    let buf = mconcat bufs
    let str = mconcat $ map fromABS bss
    Q.assert $ memEq str buf

testMemoryStimes :: ArbByteString -> Positive Int -> Property
testMemoryStimes (ABS bs) (Positive count) = monadicIO $ do
    bytes <- run $ BA.copy bs $ const $ pure ()

    let buf = stimes count bytes
    let str = stimes count bs
    Q.assert $ memEq str buf
