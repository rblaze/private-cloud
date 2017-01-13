import Control.Exception.Safe
import System.Directory
import System.Directory.Tree
import System.FilePath
import System.Posix.Files
import System.Random
import Test.Tasty
import Test.Tasty.HUnit

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
    ]

withTmpDir :: (FilePath -> FilePath -> Assertion) -> Assertion
withTmpDir func = do
    tmpbase <- getTemporaryDirectory
    tmpid <- randomIO :: IO Word
    let tmpname = "privatecloud-test-" ++ show tmpid
    let tmpdir = tmpbase </> tmpname
    bracket_
        (createDirectory tmpdir)
        (removeDirectoryRecursive tmpdir)
        (func tmpbase tmpname)

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
testMakeTree = withTmpDir $ \tmpbase tmpname -> do
    let tmpdir = tmpbase </> tmpname
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
        [ ("root/a/b/c/foo", FileInfo { fiLength = 3, fiModTime = 42 })
        , ("root/a/b/e/f/foo", FileInfo { fiLength = 4, fiModTime = 18 })
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
        [ "root/a/b/c/foo"
        , "root/a/b/foo"
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
        [ "root/a/b/c/foo" ]
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
        [ "root/a/b/e/f/foo" ]
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
        [ "root/a/b/c/pipe"
        , "root/a/b/e/f"
        , "root/a/b/e/f/foo"
        ]
        diff
