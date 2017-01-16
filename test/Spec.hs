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
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL

import PrivateCloud.Aws
import PrivateCloud.DirTree

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
        [ testCase "file HMAC" testFileHMAC
        ]
    ]

clearTree :: DirTree FileInfo -> DirTree FileInfo
clearTree = fmap step
    where
    step NotAFile = NotAFile
    step f@FileInfo{} = f{fiModTime = 0}

sampleTree :: DirTree FileInfo
sampleTree = Dir { name = "root", contents =
    [ Dir { name = "a", contents =
        [ Dir { name = "b", contents =
            [ Dir { name = "c", contents =
                [ Dir { name = "d", contents = [] }
                , File
                    { name = "foo"
                    , file = FileInfo { fiLength = 3, fiModTime = 42 }
                    }
                , File { name = "pipe", file = NotAFile }
                ]}
            , Dir { name = "e", contents =
                [ Dir { name = "f", contents =
                    [ File
                        { name = "foo"
                        , file = FileInfo { fiLength = 4, fiModTime = 18 }
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
                            , file = FileInfo { fiLength = 3, fiModTime = 0 }
                            }
                        , File { name = "pipe", file = NotAFile }
                        ]}
                    , Dir { name = "e", contents =
                        [ Dir { name = "f", contents =
                            [ File
                                { name = "foo"
                                , file = FileInfo { fiLength = 4, fiModTime = 0 }
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
        [ ("a/b/c/foo", FileInfo { fiLength = 3, fiModTime = 42 })
        , ("a/b/e/f/foo", FileInfo { fiLength = 4, fiModTime = 18 })
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
                                , file = FileInfo { fiLength = 4, fiModTime = 18 }
                                }
                            ]}
                        ]}
                    , File
                        { name = "foo"
                        , file = FileInfo { fiLength = 3, fiModTime = 42 }
                        }
                    , File { name = "pipe", file = NotAFile }
                    ]}
                ]}
            ]}
    let diff = getChangedFiles (unrollTreeFiles sampleTree) (unrollTreeFiles tree2)
    assertEqual "Incorrect change detected"
        [ "a/b/c/foo"
        , "a/b/foo"
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
                            , file = FileInfo { fiLength = 4, fiModTime = 42 }
                            }
                        , File { name = "pipe", file = NotAFile }
                        ]}
                    , Dir { name = "e", contents =
                        [ Dir { name = "f", contents =
                            [ File
                                { name = "foo"
                                , file = FileInfo { fiLength = 4, fiModTime = 18 }
                                }
                            ]}
                        ]}
                    ]}
                ]}
            ]}
    let diff = getChangedFiles (unrollTreeFiles sampleTree) (unrollTreeFiles tree2)
    assertEqual "Incorrect change detected"
        [ "a/b/c/foo" ]
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
                            , file = FileInfo { fiLength = 3, fiModTime = 42 }
                            }
                        , File { name = "pipe", file = NotAFile }
                        ]}
                    , Dir { name = "e", contents =
                        [ Dir { name = "f", contents =
                            [ File
                                { name = "foo"
                                , file = FileInfo { fiLength = 4, fiModTime = 19 }
                                }
                            ]}
                        ]}
                    ]}
                ]}
            ]}
    let diff = getChangedFiles (unrollTreeFiles sampleTree) (unrollTreeFiles tree2)
    assertEqual "Incorrect change detected"
        [ "a/b/e/f/foo" ]
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
                            , file = FileInfo { fiLength = 3, fiModTime = 42 }
                            }
                        , File
                            { name = "pipe"
                            , file = FileInfo { fiLength = 10, fiModTime = 10 }}
                        ]}
                    , Dir { name = "e", contents =
                        [ File
                            { name = "f"
                            , file = FileInfo { fiLength = 4, fiModTime = 18 }
                            }
                        ]}
                    ]}
                ]}
            ]}
    let diff = getChangedFiles (unrollTreeFiles sampleTree) (unrollTreeFiles tree2)
    assertEqual "Incorrect change detected"
        [ "a/b/c/pipe"
        , "a/b/e/f"
        , "a/b/e/f/foo"
        ]
        diff

testFileHMAC :: Assertion
testFileHMAC = withSystemTempFile "hmactest.dat" $ \filename h -> do
    BL.hPut h $ BL.take (1024 * 1024 * 3 + 150) $ BL.iterate (+ 1) 0
    hFlush h
    hmac <- bracket (openFd filename ReadOnly Nothing defaultFileFlags) closeFd getFileHash
    let strHash = show hmac
    assertEqual "HMAC mismatch" "ab78ef7a3a7b02b2ef50ee1a17e43ae0c134e0bece468b047780626264301831" strHash
    let base64 = BS8.unpack $ convertToBase Base64 hmac
    assertEqual "HMAC BASE64 mismatch" "q3jvejp7ArLvUO4aF+Q64ME04L7ORosEd4BiYmQwGDE=" base64
