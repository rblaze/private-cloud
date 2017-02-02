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
        [ testCase "getLocalChanges" testGetLocalChanges
        , testCase "getServerChanges" testGetServerChanges
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
                    LocalContentChange info -> putFileInfo conn f info
                    LocalMetadataChange info -> putFileInfo conn f info
                    LocalDelete -> deleteFileInfo conn f

    let check msg golden = do
            diff <- getChanges
            assertEqual msg golden diff
            updateDb diff

    check "incorrect change list on initial add" 
        [ ( "a/b/c/foo"
          , LocalContentChange DbFileInfo
            { dfHash = "zZx4F64Y6MG1YGUuxKDusPLIlVmILO6qaQZymdsmWmk="
            , dfLength = 3
            , dfModTime = 42
            }
          )
        , ( "a/b/e/f/foo"
          , LocalContentChange DbFileInfo
            { dfHash = "9wjg36DLTfAOSUT+NxKJmA0dCZW6bRW8pzZj+LGgN+s="
            , dfLength = 4
            , dfModTime = 42
            }
          )
        ]

    writeFile (root </> "a" </> "b" </> "c" </> "foo") "fooo"
    check "can't detect file write"
        [ ( "a/b/c/foo"
          , LocalContentChange DbFileInfo
            { dfHash = "B+9p2ru9/sTS5mdIPgWncWKBHpH76aY+p7/UaoXBlwM="
            , dfLength = 4
            , dfModTime = 42
            }
          )
        ]

    writeFile (root </> "a" </> "b" </> "c" </> "foo") "foo1"
    diff3 <- getChanges' $ \(f, i) ->
        if f == "a/b/c/foo"
            then (f, i { lfModTime = 1 })
            else (f, i)
    assertEqual "can't detect file write without len change"
        [ ( "a/b/c/foo"
          , LocalContentChange DbFileInfo
            { dfHash = "030RQSMx83MhsKJrqDbkXvlkg5KJ3hjtsSA8o3Vs0bQ="
            , dfLength = 4
            , dfModTime = 1
            }
          )
        ]
        diff3
    updateDb diff3

    check "can't detect timestamp only update"
        [ ( "a/b/c/foo"
          , LocalMetadataChange DbFileInfo
            { dfHash = "030RQSMx83MhsKJrqDbkXvlkg5KJ3hjtsSA8o3Vs0bQ="
            , dfLength = 4
            , dfModTime = 42
            }
          )
        ]

    removeFile (root </> "a" </> "b" </> "e" </> "f" </> "foo")
    check "can't detect file removal" [ ("a/b/e/f/foo", LocalDelete) ]

testGetServerChanges :: Assertion
testGetServerChanges = do
    let dbFiles =
            [ ("a/b/bar", DbFileInfo
                { dfHash = "hash1"
                , dfLength = 50
                , dfModTime = 10
                })
            , ("a/b/c/foo", DbFileInfo
                { dfHash = "hash0"
                , dfLength = 10
                , dfModTime = 1
                })
            ]

    let check msg serverFiles golden = do
            diff <- getServerChanges dbFiles serverFiles
            assertEqual msg golden diff

    check "invalid list for no changes"
        [ ("a/b/bar", CloudFile CloudFileInfo
            { cfHash = "hash1"
            , cfLength = 50
            , cfModTime = 10
            , cfVersion = VersionId "99"
            })
        , ("a/b/c/foo", CloudFile CloudFileInfo
            { cfHash = "hash0"
            , cfLength = 10
            , cfModTime = 1
            , cfVersion = VersionId "99"
            })
        ]
        []

    check "invalid list for server delete" 
        [ ("a/b/c/foo", CloudFile CloudFileInfo
            { cfHash = "hash0"
            , cfLength = 10
            , cfModTime = 1
            , cfVersion = VersionId "99"
            })
        ]
        [ ("a/b/bar", CloudDelete) ]

    check "invalid list for server marker delete" 
        [ ("a/b/bar", CloudDeleteMarker)
        , ("a/b/c/foo", CloudFile CloudFileInfo
            { cfHash = "hash0"
            , cfLength = 10
            , cfModTime = 1
            , cfVersion = VersionId "99"
            })
        ]
        [ ("a/b/bar", CloudDelete) ]

    check "invalid list for server tail delete" 
        [ ("a/b/bar", CloudFile CloudFileInfo
            { cfHash = "hash1"
            , cfLength = 50
            , cfModTime = 10
            , cfVersion = VersionId "99"
            })
        ]
        [ ("a/b/c/foo", CloudDelete) ]

    check "invalid list for server add" 
        [ ("a/b/bar", CloudFile CloudFileInfo
            { cfHash = "hash1"
            , cfLength = 50
            , cfModTime = 10
            , cfVersion = VersionId "99"
            })
        , ("a/b/bonfire", CloudFile CloudFileInfo
            { cfHash = "hash3"
            , cfLength = 20
            , cfModTime = 100
            , cfVersion = VersionId "99"
            })
        , ("a/b/c/foo", CloudFile CloudFileInfo
            { cfHash = "hash0"
            , cfLength = 10
            , cfModTime = 1
            , cfVersion = VersionId "99"
            })
        ]
        [ ("a/b/bonfire", CloudContentChange CloudFileInfo
            { cfHash = "hash3"
            , cfLength = 20
            , cfModTime = 100
            , cfVersion = VersionId "99"
            })
        ]

    check "invalid list for server tail add" 
        [ ("a/b/bar", CloudFile CloudFileInfo
            { cfHash = "hash1"
            , cfLength = 50
            , cfModTime = 10
            , cfVersion = VersionId "99"
            })
        , ("a/b/c/foo", CloudFile CloudFileInfo
            { cfHash = "hash0"
            , cfLength = 10
            , cfModTime = 1
            , cfVersion = VersionId "99"
            })
        , ("a/b/delta", CloudFile CloudFileInfo
            { cfHash = "hash14"
            , cfLength = 20
            , cfModTime = 101
            , cfVersion = VersionId "99"
            })
        ]
        [ ("a/b/delta", CloudContentChange CloudFileInfo
            { cfHash = "hash14"
            , cfLength = 20
            , cfModTime = 101
            , cfVersion = VersionId "99"
            })
        ]

    check "invalid list for server edit" 
        [ ("a/b/bar", CloudFile CloudFileInfo
            { cfHash = "hash1"
            , cfLength = 50
            , cfModTime = 10
            , cfVersion = VersionId "99"
            })
        , ("a/b/c/foo", CloudFile CloudFileInfo
            { cfHash = "hash10"
            , cfLength = 10
            , cfModTime = 17
            , cfVersion = VersionId "99"
            })
        ]
        [ ("a/b/c/foo", CloudContentChange CloudFileInfo
            { cfHash = "hash10"
            , cfLength = 10
            , cfModTime = 17
            , cfVersion = VersionId "99"
            })
        ]

    check "invalid list for server metadata change" 
        [ ("a/b/bar", CloudFile CloudFileInfo
            { cfHash = "hash1"
            , cfLength = 50
            , cfModTime = 10
            , cfVersion = VersionId "99"
            })
        , ("a/b/c/foo", CloudFile CloudFileInfo
            { cfHash = "hash0"
            , cfLength = 10
            , cfModTime = 11
            , cfVersion = VersionId "99"
            })
        ]
        [ ("a/b/c/foo", CloudMetadataChange CloudFileInfo
            { cfHash = "hash0"
            , cfLength = 10
            , cfModTime = 11
            , cfVersion = VersionId "99"
            })
        ]

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
