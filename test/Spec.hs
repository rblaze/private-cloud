{-# Language OverloadedStrings #-}
import Control.Monad
import Data.ByteArray.Encoding
import System.Directory
import System.Directory.Tree
import System.FilePath
import System.IO
import System.IO.Temp
import System.Posix.Files
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import PrivateCloud.Crypto
import PrivateCloud.DirTree
import PrivateCloud.FileInfo
import PrivateCloud.LocalDb
import PrivateCloud.Sync

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "PrivateCloud tests"
    [ testGroup "DirTree tests"
        [ testCase "makeTree" testMakeTree
        , testCase "unrollTreeFiles" testUnrollTreeFiles
        ]
    , testGroup "Crypto tests"
        [ testCase "File HMAC correct" testFileHMAC
        ]
    , testGroup "Local database tests"
        [ testCase "Add and read works" testDbAddRead
        , testCase "Double initialization don't cause data loss" testDbDoubleInit
        , testCase "Update works" testDbUpdate
        , testCase "Remove works" testDbDelete
        ]
    , testGroup "Sync tests"
        [ testCase "getLocalChanges" testGetLocalChanges
        ]
    ]

normalizeTree :: DirTree (Maybe LocalFileInfo) -> DirTree (Maybe LocalFileInfo)
normalizeTree = fmap (fmap fixModTime)
    where
    fixModTime f = f{lfModTime = 42}

sampleTree :: DirTree (Maybe LocalFileInfo)
sampleTree = Dir { name = "root", contents =
    [ Dir { name = "a", contents =
        [ Dir { name = "b", contents =
            [ Dir { name = "c", contents =
                [ Dir { name = "d", contents = [] }
                , File
                    { name = "foo"
                    , file = Just LocalFileInfo { lfLength = 3, lfModTime = 42 }
                    }
                , File { name = "pipe", file = Nothing }
                ]}
            , Dir { name = "e", contents =
                [ Dir { name = "f", contents =
                    [ File
                        { name = "foo"
                        , file = Just LocalFileInfo { lfLength = 4, lfModTime = 18 }
                        }
                    ]}
                ]}
            ]}
        ]}
    ]}

testMakeTree :: Assertion
testMakeTree = withSystemTempDirectory "privatecloud.test" $ \tmpdir -> do
    let tmpname = last $ splitDirectories tmpdir
    createDirectoryIfMissing True (tmpdir </> "a" </> "b" </> "c" </> "d")
    createDirectoryIfMissing True (tmpdir </> "a" </> "b" </> "e" </> "f")
    createNamedPipe (tmpdir </> "a" </> "b" </> "c" </> "pipe") ownerReadMode
    writeFile (tmpdir </> "a" </> "b" </> "c" </> "foo") "foo"
    writeFile (tmpdir </> "a" </> "b" </> "e" </> "f" </> "foo") "barr"
    tree <- makeTree tmpdir
    assertEqual "Incorrect tree read"
        Dir { name = tmpname, contents =
            [ Dir { name = "a", contents =
                [ Dir { name = "b", contents =
                    [ Dir { name = "c", contents =
                        [ Dir { name = "d", contents = [] }
                        , File
                            { name = "foo"
                            , file = Just LocalFileInfo { lfLength = 3, lfModTime = 42 }
                            }
                        , File { name = "pipe", file = Nothing }
                        ]}
                    , Dir { name = "e", contents =
                        [ Dir { name = "f", contents =
                            [ File
                                { name = "foo"
                                , file = Just LocalFileInfo { lfLength = 4, lfModTime = 42 }
                                }
                            ]}
                        ]}
                    ]}
                ]}
            ]}
        (normalizeTree tree)

testUnrollTreeFiles :: Assertion
testUnrollTreeFiles = do
    let files = unrollTreeFiles sampleTree
    assertEqual "Incorrect files extracted"
        [ ("a/b/c/foo", LocalFileInfo { lfLength = 3, lfModTime = 42 })
        , ("a/b/e/f/foo", LocalFileInfo { lfLength = 4, lfModTime = 18 })
        ]
        files

testFileHMAC :: Assertion
testFileHMAC = withSystemTempFile "hmactest.dat" $ \filename h -> do
    BL.hPut h $ BL.take (1024 * 1024 * 3 + 150) $ BL.iterate (+ 1) 0
    hClose h
    hmac <- getFileHash filename
    assertEqual "HMAC BASE64 mismatch" "q3jvejp7ArLvUO4aF+Q64ME04L7ORosEd4BiYmQwGDE=" hmac
    let Right decodedHMAC = convertFromBase Base64 hmac
    let printableHMAC = convertToBase Base16 $ (decodedHMAC :: BS.ByteString)
    assertEqual "HMAC mismatch" "ab78ef7a3a7b02b2ef50ee1a17e43ae0c134e0bece468b047780626264301831" (printableHMAC :: BS.ByteString)

testDbAddRead :: Assertion
testDbAddRead = withSystemTempFile "sqlite.test" $ \filename h -> do
    hClose h
    removeFile filename
    let srchash = "12345"
    let srcsize = 123
    let srcts = 9876
    [(fname, info)] <- withDatabase filename $ \conn -> do
        putFileInfo conn "foo" FileInfo
            { fiHash = srchash
            , fiLength = srcsize
            , fiModTime = srcts
            }
        getFileList conn
    assertEqual "invalid filename read" "foo" fname
    assertEqual "invalid hash read" srchash (fiHash info)
    assertEqual "invalid size read" srcsize (fiLength info)
    assertEqual "invalid modtime read" srcts (fiModTime info)

testDbDoubleInit :: Assertion
testDbDoubleInit = withSystemTempFile "sqlite.test" $ \filename h -> do
    hClose h
    removeFile filename
    let srchash = "12345"
    let srcsize = 123
    let srcts = 9876
    withDatabase filename $ \conn ->
        putFileInfo conn "foo" FileInfo
            { fiHash = srchash
            , fiLength = srcsize
            , fiModTime = srcts
            }
    [(fname, info)] <- withDatabase filename $ \conn ->
        getFileList conn
    assertEqual "invalid filename read" "foo" fname
    assertEqual "invalid hash read" srchash (fiHash info)
    assertEqual "invalid size read" srcsize (fiLength info)
    assertEqual "invalid modtime read" srcts (fiModTime info)

testDbUpdate :: Assertion
testDbUpdate = withSystemTempFile "sqlite.test" $ \filename h -> do
    hClose h
    removeFile filename
    let srchash = "12345"
    let srcsize = 123
    let srcts = 9876
    let secondHash = "78901"
    let secondSize = 1024
    let secondts = 5436
    withDatabase filename $ \conn -> do
        putFileInfo conn "foo" FileInfo
            { fiHash = srchash
            , fiLength = srcsize
            , fiModTime = srcts
            }
        putFileInfo conn "foo" FileInfo
            { fiHash = secondHash
            , fiLength = secondSize
            , fiModTime = secondts
            }
    [(fname, info)] <- withDatabase filename getFileList
    assertEqual "invalid filename read" "foo" fname
    assertEqual "invalid hash read" secondHash (fiHash info)
    assertEqual "invalid size read" secondSize (fiLength info)
    assertEqual "invalid modtime read" secondts (fiModTime info)

testDbDelete :: Assertion
testDbDelete = withSystemTempFile "sqlite.test" $ \filename h -> do
    hClose h
    removeFile filename
    let srchash = "12345"
    let srcsize = 123
    let srcts = 9876
    withDatabase filename $ \conn -> do
        v <- getFileList conn
        assertEqual "unexpected data found" [] v
        putFileInfo conn "foo" FileInfo
            { fiHash = srchash
            , fiLength = srcsize
            , fiModTime = srcts
            }
    [(fname, info)] <- withDatabase filename $ \conn -> do
        v <- getFileList conn
        deleteFileInfo conn "foo"
        return v
    assertEqual "invalid filename read" "foo" fname
    assertEqual "invalid hash read" srchash (fiHash info)
    assertEqual "invalid size read" srcsize (fiLength info)
    assertEqual "invalid modtime read" srcts (fiModTime info)

    v <- withDatabase filename getFileList
    assertEqual "data found after delete" [] v

testGetLocalChanges :: Assertion
testGetLocalChanges = withSystemTempDirectory "privatecloud.test" $ \root -> do
    createDirectoryIfMissing True (root </> "a" </> "b" </> "c" </> "d")
    createDirectoryIfMissing True (root </> "a" </> "b" </> "e" </> "f")
    createNamedPipe (root </> "a" </> "b" </> "c" </> "pipe") ownerReadMode
    writeFile (root </> "a" </> "b" </> "c" </> "foo") "foo"
    writeFile (root </> "a" </> "b" </> "e" </> "f" </> "foo") "barr"

    let getChanges' func = do
            localFiles <- map func . unrollTreeFiles . normalizeTree <$> makeTree root
            withDatabase (root </> dbName) $ \conn -> do
                dbFiles <- getFileList conn
                getLocalChanges root localFiles dbFiles

    let getChanges = getChanges' id

    let updateDb changes =
            withDatabase (root </> dbName) $ \conn ->
                forM_ changes $ \(f, i) -> case i of
                    Just v -> putFileInfo conn f v
                    Nothing -> deleteFileInfo conn f

    let check msg golden = do
            diff <- getChanges
            assertEqual msg golden diff
            updateDb diff

    check "incorrect change list on initial add" 
        [ ( "a/b/c/foo"
          , Just FileInfo
            { fiHash = "zZx4F64Y6MG1YGUuxKDusPLIlVmILO6qaQZymdsmWmk="
            , fiLength = 3
            , fiModTime = 42
            }
          )
        , ( "a/b/e/f/foo"
          , Just FileInfo
            { fiHash = "9wjg36DLTfAOSUT+NxKJmA0dCZW6bRW8pzZj+LGgN+s="
            , fiLength = 4
            , fiModTime = 42
            }
          )
        ]

    writeFile (root </> "a" </> "b" </> "c" </> "foo") "fooo"
    check "can't detect file write"
        [ ( "a/b/c/foo"
          , Just FileInfo
            { fiHash = "B+9p2ru9/sTS5mdIPgWncWKBHpH76aY+p7/UaoXBlwM="
            , fiLength = 4
            , fiModTime = 42
            }
          )
        ]

    writeFile (root </> "a" </> "b" </> "c" </> "foo") "foo1"
    diff3 <- getChanges' $ \(f, i) ->
        if f == "a" </> "b" </> "c" </> "foo"
            then (f, i { lfModTime = 1 })
            else (f, i)
    assertEqual "can't detect file write without len change"
        [ ( "a/b/c/foo"
          , Just FileInfo
            { fiHash = "030RQSMx83MhsKJrqDbkXvlkg5KJ3hjtsSA8o3Vs0bQ="
            , fiLength = 4
            , fiModTime = 1
            }
          )
        ]
        diff3
    updateDb diff3

    removeFile (root </> "a" </> "b" </> "e" </> "f" </> "foo")
    check "can't detect file removal" [ ("a/b/e/f/foo", Nothing) ]
