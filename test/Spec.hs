{-# Language OverloadedStrings #-}
import Control.Exception.Safe
import Data.ByteArray.Encoding
import System.Directory
import System.Directory.Tree
import System.FilePath
import System.IO
import System.IO.Temp
import System.Posix.Files
import System.Posix.IO
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import PrivateCloud.Crypto
import PrivateCloud.DirTree
import PrivateCloud.LocalDb

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "PrivateCloud tests"
    [ testGroup "DirTree tests"
        [ testCase "makeTree" testMakeTree
        , testCase "unrollTreeFiles" testUnrollTreeFiles
        , testCase "getChangedFiles detects move" testGetChangedFilesMove
        , testCase "getChangedFiles detects size change" testGetChangedFilesResize
        , testCase "getChangedFiles detects timestamp change" testGetChangedFilesTS
        , testCase "getChangedFiles detects type change" testGetChangedFilesType
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
    ]

clearTree :: DirTree (Maybe FileInfo) -> DirTree (Maybe FileInfo)
clearTree = fmap (fmap fixModTime)
    where
    fixModTime f = f{fiModTime = 0}

sampleTree :: DirTree (Maybe FileInfo)
sampleTree = Dir { name = "root", contents =
    [ Dir { name = "a", contents =
        [ Dir { name = "b", contents =
            [ Dir { name = "c", contents =
                [ Dir { name = "d", contents = [] }
                , File
                    { name = "foo"
                    , file = Just FileInfo { fiLength = 3, fiModTime = 42, fiHash = Nothing }
                    }
                , File { name = "pipe", file = Nothing }
                ]}
            , Dir { name = "e", contents =
                [ Dir { name = "f", contents =
                    [ File
                        { name = "foo"
                        , file = Just FileInfo { fiLength = 4, fiModTime = 18, fiHash = Nothing }
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
                            , file = Just FileInfo { fiLength = 3, fiModTime = 0, fiHash = Nothing }
                            }
                        , File { name = "pipe", file = Nothing }
                        ]}
                    , Dir { name = "e", contents =
                        [ Dir { name = "f", contents =
                            [ File
                                { name = "foo"
                                , file = Just FileInfo { fiLength = 4, fiModTime = 0, fiHash = Nothing }
                                }
                            ]}
                        ]}
                    ]}
                ]}
            ]}
        (clearTree tree)

testUnrollTreeFiles :: Assertion
testUnrollTreeFiles = do
    let files = unrollTreeFiles sampleTree
    assertEqual "Incorrect files extracted"
        [ ("a/b/c/foo", FileInfo { fiLength = 3, fiModTime = 42, fiHash = Nothing })
        , ("a/b/e/f/foo", FileInfo { fiLength = 4, fiModTime = 18, fiHash = Nothing })
        ]
        files

testGetChangedFilesMove :: Assertion
testGetChangedFilesMove = do
    let tree2 = Dir { name = "root", contents =
            [ Dir { name = "a", contents =
                [ Dir { name = "b", contents =
                    [ Dir { name = "c", contents =
                        [ Dir { name = "d", contents = [] }
                        ]}
                    , Dir { name = "e", contents =
                        [ Dir { name = "f", contents =
                            [ File
                                { name = "foo"
                                , file = Just FileInfo { fiLength = 4, fiModTime = 18, fiHash = Nothing }
                                }
                            ]}
                        ]}
                    , File
                        { name = "foo"
                        , file = Just FileInfo { fiLength = 3, fiModTime = 42, fiHash = Nothing }
                        }
                    , File { name = "pipe", file = Nothing }
                    ]}
                ]}
            ]}
    let diff = getChangedFiles (unrollTreeFiles sampleTree) (unrollTreeFiles tree2)
    assertEqual "Incorrect change detected"
        [ ("a/b/c/foo" , Just FileInfo { fiLength = 3, fiModTime = 42, fiHash = Nothing }, Nothing)
        , ("a/b/foo", Nothing, Just FileInfo { fiLength = 3, fiModTime = 42, fiHash = Nothing })
        ]
        diff

testGetChangedFilesResize :: Assertion
testGetChangedFilesResize = do
    let tree2 = Dir { name = "root", contents =
            [ Dir { name = "a", contents =
                [ Dir { name = "b", contents =
                    [ Dir { name = "c", contents =
                        [ Dir { name = "d", contents = [] }
                        , File
                            { name = "foo"
                            , file = Just FileInfo { fiLength = 4, fiModTime = 42, fiHash = Nothing }
                            }
                        , File { name = "pipe", file = Nothing }
                        ]}
                    , Dir { name = "e", contents =
                        [ Dir { name = "f", contents =
                            [ File
                                { name = "foo"
                                , file = Just FileInfo { fiLength = 4, fiModTime = 18, fiHash = Nothing }
                                }
                            ]}
                        ]}
                    ]}
                ]}
            ]}
    let diff = getChangedFiles (unrollTreeFiles sampleTree) (unrollTreeFiles tree2)
    assertEqual "Incorrect change detected"
        [ ("a/b/c/foo" , Just FileInfo { fiLength = 3, fiModTime = 42, fiHash = Nothing }, Just FileInfo { fiLength = 4, fiModTime = 42, fiHash = Nothing }) ]
        diff

testGetChangedFilesTS :: Assertion
testGetChangedFilesTS = do
    let tree2 = Dir { name = "root", contents =
            [ Dir { name = "a", contents =
                [ Dir { name = "b", contents =
                    [ Dir { name = "c", contents =
                        [ Dir { name = "d", contents = [] }
                        , File
                            { name = "foo"
                            , file = Just FileInfo { fiLength = 3, fiModTime = 42, fiHash = Nothing }
                            }
                        , File { name = "pipe", file = Nothing }
                        ]}
                    , Dir { name = "e", contents =
                        [ Dir { name = "f", contents =
                            [ File
                                { name = "foo"
                                , file = Just FileInfo { fiLength = 4, fiModTime = 19, fiHash = Nothing }
                                }
                            ]}
                        ]}
                    ]}
                ]}
            ]}
    let diff = getChangedFiles (unrollTreeFiles sampleTree) (unrollTreeFiles tree2)
    assertEqual "Incorrect change detected"
        [ ("a/b/e/f/foo", Just FileInfo { fiLength = 4, fiModTime = 18, fiHash = Nothing }, Just FileInfo { fiLength = 4, fiModTime = 19, fiHash = Nothing }) ]
        diff

testGetChangedFilesType :: Assertion
testGetChangedFilesType = do
    let tree2 = Dir { name = "root", contents =
            [ Dir { name = "a", contents =
                [ Dir { name = "b", contents =
                    [ Dir { name = "c", contents =
                        [ Dir { name = "d", contents = [] }
                        , File
                            { name = "foo"
                            , file = Just FileInfo { fiLength = 3, fiModTime = 42, fiHash = Nothing }
                            }
                        , File
                            { name = "pipe"
                            , file = Just FileInfo { fiLength = 10, fiModTime = 10, fiHash = Nothing }}
                        ]}
                    , Dir { name = "e", contents =
                        [ File
                            { name = "f"
                            , file = Just FileInfo { fiLength = 4, fiModTime = 18, fiHash = Nothing }
                            }
                        ]}
                    ]}
                ]}
            ]}
    let diff = getChangedFiles (unrollTreeFiles sampleTree) (unrollTreeFiles tree2)
    assertEqual "Incorrect change detected"
        [ ("a/b/c/pipe", Nothing, Just FileInfo { fiLength = 10, fiModTime = 10, fiHash = Nothing })
        , ("a/b/e/f", Nothing, Just FileInfo { fiLength = 4, fiModTime = 18, fiHash = Nothing })
        , ("a/b/e/f/foo", Just FileInfo { fiLength = 4, fiModTime = 18, fiHash = Nothing }, Nothing)
        ]
        diff

testFileHMAC :: Assertion
testFileHMAC = withSystemTempFile "hmactest.dat" $ \filename h -> do
    BL.hPut h $ BL.take (1024 * 1024 * 3 + 150) $ BL.iterate (+ 1) 0
    hFlush h
    hmac <- bracket (openFd filename ReadOnly Nothing defaultFileFlags) closeFd getFileHash
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
    withConnection filename $ \conn -> do
        initDatabase conn
        withTransaction conn $
            putFileInfo conn "foo" srchash srcsize
    Just (hash, size) <- withConnection filename $ \conn -> do
        getFileInfo conn "foo"
    assertEqual "invalid hash read" srchash hash
    assertEqual "invalid size read" srcsize size

testDbDoubleInit :: Assertion
testDbDoubleInit = withSystemTempFile "sqlite.test" $ \filename h -> do
    hClose h
    removeFile filename
    let srchash = "12345"
    let srcsize = 123
    withConnection filename $ \conn -> do
        initDatabase conn
        withTransaction conn $
            putFileInfo conn "foo" srchash srcsize
    Just (hash, size) <- withConnection filename $ \conn -> do
        initDatabase conn
        getFileInfo conn "foo"
    assertEqual "invalid hash read" srchash hash
    assertEqual "invalid size read" srcsize size

testDbUpdate :: Assertion
testDbUpdate = withSystemTempFile "sqlite.test" $ \filename h -> do
    hClose h
    removeFile filename
    let srchash = "12345"
    let srcsize = 123
    let secondHash = "78901"
    let secondSize = 1024
    withConnection filename $ \conn -> do
        initDatabase conn
        withTransaction conn $
            putFileInfo conn "foo" srchash srcsize
        withTransaction conn $
            putFileInfo conn "foo" secondHash secondSize
    Just (hash, size) <- withConnection filename $ \conn -> do
        getFileInfo conn "foo"
    assertEqual "invalid hash read" secondHash hash
    assertEqual "invalid size read" secondSize size

testDbDelete :: Assertion
testDbDelete = withSystemTempFile "sqlite.test" $ \filename h -> do
    hClose h
    removeFile filename
    let srchash = "12345"
    let srcsize = 123
    withConnection filename $ \conn -> do
        initDatabase conn
        v <- getFileInfo conn "foo"
        assertEqual "unexpected data found" Nothing v
        withTransaction conn $
            putFileInfo conn "foo" srchash srcsize
    Just (hash, size) <- withConnection filename $ \conn -> do
        v <- getFileInfo conn "foo"
        withTransaction conn $
            deleteFileInfo conn "foo"
        return v
    assertEqual "invalid hash read" srchash hash
    assertEqual "invalid size read" srcsize size

    v <- withConnection filename $ \conn ->
        getFileInfo conn "foo"
    assertEqual "data found after delete" Nothing v
