{-# Language OverloadedStrings, LambdaCase, RecordWildCards #-}
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

import PrivateCloud.Aws
import PrivateCloud.Crypto
import PrivateCloud.DirTree
import PrivateCloud.FileInfo
import PrivateCloud.LocalDb
import PrivateCloud.Sync

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "PrivateCloud tests"
    [ testGroup "Helper function tests"
        [ testCase "zipLists3" testZipLists3
        ]
    , testGroup "DirTree tests"
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
        [ testCase "getFileChanges" testGetFileChanges
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
    let printableHMAC = convertToBase Base16 (decodedHMAC :: BS.ByteString)
    assertEqual "HMAC mismatch" "ab78ef7a3a7b02b2ef50ee1a17e43ae0c134e0bece468b047780626264301831" (printableHMAC :: BS.ByteString)

testDbAddRead :: Assertion
testDbAddRead = withSystemTempFile "sqlite.test" $ \filename h -> do
    hClose h
    removeFile filename
    let srchash = "12345"
    let srcsize = 123
    let srcts = 9876
    [(fname, info)] <- withDatabase filename $ \conn -> do
        putFileInfo conn "foo" DbFileInfo
            { dfHash = srchash
            , dfLength = srcsize
            , dfModTime = srcts
            }
        getFileList conn
    assertEqual "invalid filename read" "foo" fname
    assertEqual "invalid hash read" srchash (dfHash info)
    assertEqual "invalid size read" srcsize (dfLength info)
    assertEqual "invalid modtime read" srcts (dfModTime info)

testDbDoubleInit :: Assertion
testDbDoubleInit = withSystemTempFile "sqlite.test" $ \filename h -> do
    hClose h
    removeFile filename
    let srchash = "12345"
    let srcsize = 123
    let srcts = 9876
    withDatabase filename $ \conn ->
        putFileInfo conn "foo" DbFileInfo
            { dfHash = srchash
            , dfLength = srcsize
            , dfModTime = srcts
            }
    [(fname, info)] <- withDatabase filename $ \conn ->
        getFileList conn
    assertEqual "invalid filename read" "foo" fname
    assertEqual "invalid hash read" srchash (dfHash info)
    assertEqual "invalid size read" srcsize (dfLength info)
    assertEqual "invalid modtime read" srcts (dfModTime info)

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
        putFileInfo conn "foo" DbFileInfo
            { dfHash = srchash
            , dfLength = srcsize
            , dfModTime = srcts
            }
        putFileInfo conn "foo" DbFileInfo
            { dfHash = secondHash
            , dfLength = secondSize
            , dfModTime = secondts
            }
    [(fname, info)] <- withDatabase filename getFileList
    assertEqual "invalid filename read" "foo" fname
    assertEqual "invalid hash read" secondHash (dfHash info)
    assertEqual "invalid size read" secondSize (dfLength info)
    assertEqual "invalid modtime read" secondts (dfModTime info)

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
        putFileInfo conn "foo" DbFileInfo
            { dfHash = srchash
            , dfLength = srcsize
            , dfModTime = srcts
            }
    [(fname, info)] <- withDatabase filename $ \conn -> do
        v <- getFileList conn
        deleteFileInfo conn "foo"
        return v
    assertEqual "invalid filename read" "foo" fname
    assertEqual "invalid hash read" srchash (dfHash info)
    assertEqual "invalid size read" srcsize (dfLength info)
    assertEqual "invalid modtime read" srcts (dfModTime info)

    v <- withDatabase filename getFileList
    assertEqual "data found after delete" [] v

testGetFileChanges :: Assertion
testGetFileChanges = withSystemTempDirectory "privatecloud.test" $ \root -> withDatabase (root </> dbName) $ \conn -> do
    createDirectoryIfMissing True (root </> "a" </> "b" </> "c" </> "d")
    createDirectoryIfMissing True (root </> "a" </> "b" </> "e" </> "f")
    createNamedPipe (root </> "a" </> "b" </> "c" </> "pipe") ownerReadMode
    writeFile (root </> "a" </> "b" </> "c" </> "foo") "foo"
    writeFile (root </> "a" </> "b" </> "e" </> "f" </> "foo") "barr"

    let getChanges' func serverFiles = do
            localFiles <- map func . unrollTreeFiles . normalizeTree <$> makeTree root
            dbFiles <- getFileList conn
            getFileChanges root localFiles dbFiles serverFiles

    let getChanges = getChanges' id

    let cloud2db CloudFileInfo{..} = DbFileInfo
            { dfLength = cfLength
            , dfModTime = cfModTime
            , dfHash = cfHash
            }

    let local2db filename LocalFileInfo{..} = do
            hash <- getFileHash (root </> filename)
            return DbFileInfo
                { dfHash = hash
                , dfLength = lfLength
                , dfModTime = lfModTime
                }

    let updateDb changes =
            forM_ changes $ \case
                UpdateLocalFile{..} -> putFileInfo conn faFilename (cloud2db faCloudInfo)
                UpdateLocalMetadata{..} -> putFileInfo conn faFilename (cloud2db faCloudInfo)
                DeleteLocalFile{..} -> deleteFileInfo conn faFilename
                UpdateCloudFile{..} -> do
                    info <- local2db faFilename faLocalInfo
                    putFileInfo conn faFilename info
                UpdateCloudMetadata{..} -> do
                    info <- local2db faFilename faLocalInfo
                    putFileInfo conn faFilename info
                DeleteCloudFile{..} -> deleteFileInfo conn faFilename
                _ -> return ()

    let check msg server golden = do
            diff <- getChanges server
            assertEqual msg golden diff
            updateDb diff

    check "incorrect change list on initial add" []
        [ UpdateCloudFile
          { faFilename = "a/b/c/foo"
          , faLocalInfo = LocalFileInfo
            { lfLength = 3
            , lfModTime = 42
            }
          }
        , UpdateCloudFile
          { faFilename = "a/b/e/f/foo"
          , faLocalInfo = LocalFileInfo
            { lfLength = 4
            , lfModTime = 42
            }
          }
        ]

    check "can't detect absense of changes"
        [ ( "a/b/c/foo"
          , CloudFile CloudFileInfo
            { cfHash = "zZx4F64Y6MG1YGUuxKDusPLIlVmILO6qaQZymdsmWmk="
            , cfLength = 3
            , cfModTime = 42
            , cfVersion = VersionId "100"
            }
          )
        , ( "a/b/e/f/foo"
          , CloudFile CloudFileInfo
            { cfHash = "9wjg36DLTfAOSUT+NxKJmA0dCZW6bRW8pzZj+LGgN+s="
            , cfLength = 4
            , cfModTime = 42
            , cfVersion = VersionId "101"
            }
          )
        ]
        []

    writeFile (root </> "a" </> "b" </> "c" </> "foo") "fooo"
    check "can't detect file write"
        [ ( "a/b/c/foo"
          , CloudFile CloudFileInfo
            { cfHash = "zZx4F64Y6MG1YGUuxKDusPLIlVmILO6qaQZymdsmWmk="
            , cfLength = 3
            , cfModTime = 42
            , cfVersion = VersionId "100"
            }
          )
        , ( "a/b/e/f/foo"
          , CloudFile CloudFileInfo
            { cfHash = "9wjg36DLTfAOSUT+NxKJmA0dCZW6bRW8pzZj+LGgN+s="
            , cfLength = 4
            , cfModTime = 42
            , cfVersion = VersionId "101"
            }
          )
        ]
        [ UpdateCloudFile
          { faFilename = "a/b/c/foo"
          , faLocalInfo = LocalFileInfo
            { lfLength = 4
            , lfModTime = 42
            }
          }
        ]

    writeFile (root </> "a" </> "b" </> "c" </> "foo") "foo1"
    diff3 <- getChanges'
        ( \(f, i) -> if f == "a/b/c/foo"
                        then (f, i { lfModTime = 1 })
                        else (f, i)
        )
        [ ( "a/b/c/foo"
          , CloudFile CloudFileInfo
            { cfHash = "B+9p2ru9/sTS5mdIPgWncWKBHpH76aY+p7/UaoXBlwM="
            , cfLength = 4
            , cfModTime = 42
            , cfVersion = VersionId "105"
            }
          )
        , ( "a/b/e/f/foo"
          , CloudFile CloudFileInfo
            { cfHash = "9wjg36DLTfAOSUT+NxKJmA0dCZW6bRW8pzZj+LGgN+s="
            , cfLength = 4
            , cfModTime = 42
            , cfVersion = VersionId "101"
            }
          )
        ]
    assertEqual "can't detect file write without len change"
        [ UpdateCloudFile
          { faFilename = "a/b/c/foo"
          , faLocalInfo = LocalFileInfo
            { lfLength = 4
            , lfModTime = 1
            }
          }
        ]
        diff3
    updateDb diff3

    check "can't detect timestamp only update"
        [ ( "a/b/c/foo"
          , CloudFile CloudFileInfo
            { cfHash = "030RQSMx83MhsKJrqDbkXvlkg5KJ3hjtsSA8o3Vs0bQ="
            , cfLength = 4
            , cfModTime = 1
            , cfVersion = VersionId "108"
            }
          )
        , ( "a/b/e/f/foo"
          , CloudFile CloudFileInfo
            { cfHash = "9wjg36DLTfAOSUT+NxKJmA0dCZW6bRW8pzZj+LGgN+s="
            , cfLength = 4
            , cfModTime = 42
            , cfVersion = VersionId "101"
            }
          )
        ]
        [ UpdateCloudMetadata
          { faFilename = "a/b/c/foo"
          , faLocalInfo = LocalFileInfo
            { lfLength = 4
            , lfModTime = 42
            }
          , faExpectedHash = "030RQSMx83MhsKJrqDbkXvlkg5KJ3hjtsSA8o3Vs0bQ="
          }
        ]

    removeFile (root </> "a" </> "b" </> "e" </> "f" </> "foo")
    check "can't detect file removal"
        [ ( "a/b/c/foo"
          , CloudFile CloudFileInfo
            { cfHash = "030RQSMx83MhsKJrqDbkXvlkg5KJ3hjtsSA8o3Vs0bQ="
            , cfLength = 4
            , cfModTime = 42
            , cfVersion = VersionId "108"
            }
          )
        , ( "a/b/e/f/foo"
          , CloudFile CloudFileInfo
            { cfHash = "9wjg36DLTfAOSUT+NxKJmA0dCZW6bRW8pzZj+LGgN+s="
            , cfLength = 4
            , cfModTime = 42
            , cfVersion = VersionId "101"
            }
          )
        ]
        [ DeleteCloudFile
          { faFilename = "a/b/e/f/foo"
          }
        ]

    check "can't detect server add"
        [ ( "a/b/c/foo"
          , CloudFile CloudFileInfo
            { cfHash = "030RQSMx83MhsKJrqDbkXvlkg5KJ3hjtsSA8o3Vs0bQ="
            , cfLength = 4
            , cfModTime = 42
            , cfVersion = VersionId "108"
            }
          )
        , ( "a/b/e/buzz"
          , CloudFile CloudFileInfo
            { cfHash = "9wjg36DLTfAOSUT+NxKJmA0dCZW6bRW8pzZj+LGgN+s="
            , cfLength = 4
            , cfModTime = 42
            , cfVersion = VersionId "1"
            }
          )
        ]
        [ UpdateLocalFile
          { faFilename = "a/b/e/buzz"
          , faCloudInfo = CloudFileInfo
            { cfHash = "9wjg36DLTfAOSUT+NxKJmA0dCZW6bRW8pzZj+LGgN+s="
            , cfLength = 4
            , cfModTime = 42
            , cfVersion = VersionId "1"
            }
          }
        ]
    writeFile (root </> "a" </> "b" </> "e" </> "buzz") "barr"

    check "can't detect server edit"
        [ ( "a/b/c/foo"
          , CloudFile CloudFileInfo
            { cfHash = "030RQSMx83MhsKJrqDbkXvlkg5KJ3hjtsSA8o3Vs0bQ="
            , cfLength = 4
            , cfModTime = 42
            , cfVersion = VersionId "108"
            }
          )
        , ( "a/b/e/buzz"
          , CloudFile CloudFileInfo
            { cfHash = "B+9p2ru9/sTS5mdIPgWncWKBHpH76aY+p7/UaoXBlwM="
            , cfLength = 4
            , cfModTime = 42
            , cfVersion = VersionId "2"
            }
          )
        ]
        [ UpdateLocalFile
          { faFilename = "a/b/e/buzz"
          , faCloudInfo = CloudFileInfo
            { cfHash = "B+9p2ru9/sTS5mdIPgWncWKBHpH76aY+p7/UaoXBlwM="
            , cfLength = 4
            , cfModTime = 42
            , cfVersion = VersionId "2"
            }
          }
        ]
    writeFile (root </> "a" </> "b" </> "e" </> "buzz") "fooo"

    check "can't detect server metadata change"
        [ ( "a/b/c/foo"
          , CloudFile CloudFileInfo
            { cfHash = "030RQSMx83MhsKJrqDbkXvlkg5KJ3hjtsSA8o3Vs0bQ="
            , cfLength = 4
            , cfModTime = 42
            , cfVersion = VersionId "108"
            }
          )
        , ( "a/b/e/buzz"
          , CloudFile CloudFileInfo
            { cfHash = "B+9p2ru9/sTS5mdIPgWncWKBHpH76aY+p7/UaoXBlwM="
            , cfLength = 4
            , cfModTime = 50
            , cfVersion = VersionId "2"
            }
          )
        ]
        [ UpdateLocalMetadata
          { faFilename = "a/b/e/buzz"
          , faCloudInfo = CloudFileInfo
            { cfHash = "B+9p2ru9/sTS5mdIPgWncWKBHpH76aY+p7/UaoXBlwM="
            , cfLength = 4
            , cfModTime = 50
            , cfVersion = VersionId "2"
            }
          }
        ]

    check "can't detect server delete with marker"
        [ ( "a/b/c/foo"
          , CloudFile CloudFileInfo
            { cfHash = "030RQSMx83MhsKJrqDbkXvlkg5KJ3hjtsSA8o3Vs0bQ="
            , cfLength = 4
            , cfModTime = 42
            , cfVersion = VersionId "108"
            }
          )
        , ( "a/b/e/buzz"
          , CloudDeleteMarker
          )
        ]
        [ DeleteLocalFile
          { faFilename = "a/b/e/buzz"
          }
        ]
    removeFile (root </> "a" </> "b" </> "e" </> "buzz")

    check "can't detect server delete with missing record"
        [ ( "a/b/e/buzz"
          , CloudDeleteMarker
          )
        ]
        [ DeleteLocalFile
          { faFilename = "a/b/c/foo"
          }
        ]
    removeFile (root </> "a" </> "b" </> "c" </> "foo")

testZipLists3 :: Assertion
testZipLists3 = do
    assertEqual "test1"
        [ (1, Just "1a", Just "1b", Nothing)
        , (2, Just "2a", Nothing, Nothing)
        , (3, Nothing, Just "3b", Nothing)
        , (4, Nothing, Nothing, Just "4c")
        , (5, Just "5a", Just "5b", Just "5c")
        , (6, Just "6a", Nothing, Just "6c")
        , (8, Nothing, Just "8b", Just "8c")
        , (9, Nothing, Nothing, Just "9c")
        ]
        (zipLists3
            [ (1 :: Int, "1a" :: String)
            , (2, "2a")
            , (5, "5a")
            , (6, "6a")
            ]
            [ (1, "1b" :: String)
            , (3, "3b")
            , (5, "5b")
            , (8, "8b")
            ]
            [ (4, "4c" :: String)
            , (5, "5c")
            , (6, "6c")
            , (8, "8c")
            , (9, "9c")
            ]
        )
