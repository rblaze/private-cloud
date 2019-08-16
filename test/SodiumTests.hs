{-# LANGUAGE OverloadedStrings #-}
module SodiumTests (sodiumTests) where

import Data.ByteArray.Encoding
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import Sodium.Hash

sodiumTests :: TestTree
sodiumTests = testGroup "Sodium tests"
    [ testCase "simple hash" testHash
    , testCase "stream hash" testStreamHash
    ]

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
