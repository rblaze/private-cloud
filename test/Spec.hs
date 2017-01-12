import Control.Exception.Safe
import System.Directory
import System.Directory.Tree
import System.FilePath
import System.Random
import Test.Tasty
import Test.Tasty.HUnit

import PrivateCloud.DirTree

main :: IO ()
main = defaultMain tests


tests :: TestTree
tests = testGroup "PrivateCloud tests"
    [ testGroup "DirTree tests"
        [ testCase "check tree creation" checkTreeCreation
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

clearTree :: AnchoredDirTree FileInfo -> AnchoredDirTree FileInfo
clearTree (root :/ tree) = root :/ go tree
    where
    go f@Failed{} = f
    go d@Dir{contents = cs} = d{contents = map go cs}
    go f@File{file = fi} = f{file = fi{fiModTime = 0}}

checkTreeCreation :: Assertion
checkTreeCreation = withTmpDir $ \tmpbase tmpname -> do
    let tmpdir = tmpbase </> tmpname
    createDirectoryIfMissing True (tmpdir </> "a" </> "b" </> "c" </> "d")
    createDirectoryIfMissing True (tmpdir </> "a" </> "b" </> "e" </> "f")
    writeFile (tmpdir </> "a" </> "b" </> "c" </> "foo") "foo"
    tree <- makeTree tmpdir
    assertEqual "Incorrect tree read"
        (tmpbase :/ Dir
            { name = tmpname
            , contents =
                [ Dir
                    { name = "a"
                    , contents =
                        [ Dir
                            { name = "b"
                            , contents =
                                [ Dir
                                    { name = "e"
                                    , contents =
                                        [ Dir { name = "f" , contents = [] }
                                        ]
                                    }
                                , Dir
                                    { name = "c"
                                    , contents =
                                        [ Dir { name = "d" , contents = [] }
                                        , File
                                            { name = "foo"
                                            , file = FileInfo
                                                { fiLength = 3
                                                , fiModTime = 0
                                                }
                                            }
                                        ]
                                    }
                                ]
                            }
                        ]
                    }
                ]
            }
        )
        (clearTree tree)
