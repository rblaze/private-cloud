{-# Language CPP, OverloadedStrings, LambdaCase, RecordWildCards #-}
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteArray as BA
import Data.ByteArray.Encoding
import System.Directory
import System.Directory.Tree
import System.FilePath
import System.FilePath.Glob
import System.IO
import System.IO.Temp
#ifndef WINBUILD
import System.Posix.Files
#endif
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as T

import PrivateCloud.Cloud.Crypto
import PrivateCloud.Cloud.DirTree
import PrivateCloud.Cloud.LocalDb
import PrivateCloud.Cloud.Monad
import PrivateCloud.Cloud.Sync
import PrivateCloud.Provider.FileInfo

import Provider

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "PrivateCloud tests"
    [ testGroup "Helper function tests"
        [ testCase "zipLists3" testZipLists3
        , testCase "path2entry" testPath2Entry
        , testCase "entry2path" testEntry2Path
        , testCase "entryFile" testEntryFile
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
        , testCase "Update works" testDbUpdate
        , testCase "Remove works" testDbDelete
        ]
    , testGroup "Sync tests"
        [ testCase "getFileChanges" testGetFileChanges
        ]
    ]

dbPattern :: Pattern
dbPattern = compile dbName

normalizeTree :: DirTree (Maybe LocalFileInfo) -> DirTree (Maybe LocalFileInfo)
normalizeTree = fmap (fmap fixModTime)
    where
    fixModTime f = f{lfModTime = Timestamp 42}

sampleTree :: DirTree (Maybe LocalFileInfo)
sampleTree = Dir { name = "root", contents =
    [ Dir { name = "a", contents =
        [ Dir { name = "b", contents =
            [ Dir { name = "c", contents =
                [ Dir { name = "d", contents = [] }
                , File
                    { name = "foo"
                    , file = Just LocalFileInfo
                        { lfLength = 3
                        , lfModTime = Timestamp 42
                        }
                    }
#ifndef WINBUILD
                , File { name = "pipe", file = Nothing }
#endif
                ]}
            , Dir { name = "e", contents =
                [ Dir { name = "f", contents =
                    [ File
                        { name = "foo"
                        , file = Just LocalFileInfo
                            { lfLength = 4
                            , lfModTime = Timestamp 18
                            }
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
#ifndef WINBUILD
    createNamedPipe (tmpdir </> "a" </> "b" </> "c" </> "pipe") ownerReadMode
#endif
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
                            , file = Just LocalFileInfo { lfLength = 3, lfModTime = Timestamp 42 }
                            }
#ifndef WINBUILD
                        , File { name = "pipe", file = Nothing }
#endif
                        ]}
                    , Dir { name = "e", contents =
                        [ Dir { name = "f", contents =
                            [ File
                                { name = "foo"
                                , file = Just LocalFileInfo { lfLength = 4, lfModTime = Timestamp 42 }
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
        [ (EntryName "a/b/c/foo", LocalFileInfo { lfLength = 3, lfModTime = Timestamp 42 })
        , (EntryName "a/b/e/f/foo", LocalFileInfo { lfLength = 4, lfModTime = Timestamp 18 })
        ]
        files

testFileHMAC :: Assertion
testFileHMAC = withSystemTempFile "hmactest.dat" $ \filename h -> do
    BL.hPut h $ BL.take (1024 * 1024 * 3 + 150) $ BL.iterate (+ 1) 0
    hClose h
    hmac <- getFileHash filename
    assertEqual "HMAC BASE64 mismatch" (Hash "q3jvejp7ArLvUO4aF+Q64ME04L7ORosEd4BiYmQwGDE=") hmac
    let Right decodedHMAC = convertFromBase Base64 $ T.encodeUtf8 $ hash2text hmac
    let printableHMAC = convertToBase Base16 (decodedHMAC :: BS.ByteString)
    assertEqual "HMAC mismatch" "ab78ef7a3a7b02b2ef50ee1a17e43ae0c134e0bece468b047780626264301831" (printableHMAC :: BS.ByteString)

testDbAddRead :: Assertion
testDbAddRead = withSystemTempDirectory "sqlite.test" $ \tmpdir -> do
    let srchash = Hash "12345"
    let srcsize = 123
    let srcts = Timestamp 9876
    _ <- setupTestCloud tmpdir "foobar"
    [(fname, info)] <- runTestCloud tmpdir [] (const $ return $ Just BA.empty) $ do
        putFileInfo (EntryName "foo") DbFileInfo
            { dfHash = srchash
            , dfLength = srcsize
            , dfModTime = srcts
            }
        getFileList
    assertEqual "invalid filename read" (EntryName "foo") fname
    assertEqual "invalid hash read" srchash (dfHash info)
    assertEqual "invalid size read" srcsize (dfLength info)
    assertEqual "invalid modtime read" srcts (dfModTime info)

testDbUpdate :: Assertion
testDbUpdate = withSystemTempDirectory "sqlite.test" $ \tmpdir -> do
    let srchash = Hash "12345"
    let srcsize = 123
    let srcts = Timestamp 9876
    let secondHash = Hash "78901"
    let secondSize = 1024
    let secondts = Timestamp 5436
    _ <- setupTestCloud tmpdir "foobar"
    runTestCloud tmpdir [] (const $ return $ Just BA.empty) $ do
        putFileInfo (EntryName "foo") DbFileInfo
            { dfHash = srchash
            , dfLength = srcsize
            , dfModTime = srcts
            }
        putFileInfo (EntryName "foo") DbFileInfo
            { dfHash = secondHash
            , dfLength = secondSize
            , dfModTime = secondts
            }
    [(fname, info)] <- runTestCloud tmpdir [] (const $ return $ Just BA.empty) getFileList
    assertEqual "invalid filename read" (EntryName "foo") fname
    assertEqual "invalid hash read" secondHash (dfHash info)
    assertEqual "invalid size read" secondSize (dfLength info)
    assertEqual "invalid modtime read" secondts (dfModTime info)

testDbDelete :: Assertion
testDbDelete = withSystemTempDirectory "sqlite.test" $ \tmpdir -> do
    let srchash = Hash "12345"
    let srcsize = 123
    let srcts = Timestamp 9876
    _ <- setupTestCloud tmpdir "foobar"
    runTestCloud tmpdir [] (const $ return $ Just BA.empty) $ do
        v <- getFileList
        liftIO $ assertEqual "unexpected data found" [] v
        putFileInfo (EntryName "foo") DbFileInfo
            { dfHash = srchash
            , dfLength = srcsize
            , dfModTime = srcts
            }
    [(fname, info)] <- runTestCloud tmpdir [] (const $ return $ Just BA.empty) $ do
        v <- getFileList
        deleteFileInfo (EntryName "foo")
        return v
    assertEqual "invalid filename read" (EntryName "foo") fname
    assertEqual "invalid hash read" srchash (dfHash info)
    assertEqual "invalid size read" srcsize (dfLength info)
    assertEqual "invalid modtime read" srcts (dfModTime info)

    v <- runTestCloud tmpdir [] (const $ return $ Just BA.empty) getFileList
    assertEqual "data found after delete" [] v

testGetFileChanges :: Assertion
testGetFileChanges = withSystemTempDirectory "privatecloud.test" $ \root -> do
    _ <- setupTestCloud root "foobar"
    createDirectoryIfMissing True (root </> "a" </> "b" </> "c" </> "d")
    createDirectoryIfMissing True (root </> "a" </> "b" </> "e" </> "f")
#ifndef WINBUILD
    createNamedPipe (root </> "a" </> "b" </> "c" </> "pipe") ownerReadMode
#endif
    writeFile (root </> "a" </> "b" </> "c" </> "foo") "foo"
    writeFile (root </> "a" </> "b" </> "e" </> "f" </> "foo") "barr"

    let getChanges' func serverFiles = do
            localFiles <- map func . unrollTreeFiles . normalizeTree <$> makeTree root
            runTestCloud root [dbPattern] (const $ return $ Just BA.empty) $ do
                dbFiles <- getFileList
                getAllFileChanges localFiles dbFiles serverFiles

    let getChanges = getChanges' id

    let cloud2db CloudFileInfo{..} = DbFileInfo
            { dfLength = cfLength
            , dfModTime = cfModTime
            , dfHash = cfHash
            }

    let local2db filename LocalFileInfo{..} = do
            hash <- liftIO $ getFileHash (root </> entry2path filename)
            return DbFileInfo
                { dfHash = hash
                , dfLength = lfLength
                , dfModTime = lfModTime
                }

    let updateDb changes =
            runTestCloud root [dbPattern] (const $ return $ Just BA.empty) $ forM_ changes $ \case
                UpdateLocalFile{..} -> putFileInfo faFilename (cloud2db faCloudInfo)
                UpdateLocalMetadata{..} -> putFileInfo faFilename (cloud2db faCloudInfo)
                DeleteLocalFile{..} -> deleteFileInfo faFilename
                UpdateCloudFile{..} -> do
                    info <- local2db faFilename faLocalInfo
                    putFileInfo faFilename info
                UpdateCloudMetadata{..} -> do
                    info <- local2db faFilename faLocalInfo
                    putFileInfo faFilename info
                DeleteCloudFile{..} -> deleteFileInfo faFilename
                _ -> return ()

    let check msg server golden = do
            diff <- getChanges server
            assertEqual msg golden diff
            updateDb diff

    check "incorrect change list on initial add" []
        [ UpdateCloudFile
          { faFilename = EntryName "a/b/c/foo"
          , faLocalInfo = LocalFileInfo
            { lfLength = 3
            , lfModTime = Timestamp 42
            }
          }
        , UpdateCloudFile
          { faFilename = EntryName "a/b/e/f/foo"
          , faLocalInfo = LocalFileInfo
            { lfLength = 4
            , lfModTime = Timestamp 42
            }
          }
        ]

    check "can't detect absense of changes"
        [ ( EntryName "a/b/c/foo"
          , CloudFile CloudFileInfo
            { cfHash = Hash "zZx4F64Y6MG1YGUuxKDusPLIlVmILO6qaQZymdsmWmk="
            , cfLength = 3
            , cfModTime = Timestamp 42
            , cfVersion = VersionId "100"
            }
          )
        , ( EntryName "a/b/e/f/foo"
          , CloudFile CloudFileInfo
            { cfHash = Hash "9wjg36DLTfAOSUT+NxKJmA0dCZW6bRW8pzZj+LGgN+s="
            , cfLength = 4
            , cfModTime = Timestamp 42
            , cfVersion = VersionId "101"
            }
          )
        ]
        []

    writeFile (root </> "a" </> "b" </> "c" </> "foo") "fooo"
    check "can't detect file write"
        [ ( EntryName "a/b/c/foo"
          , CloudFile CloudFileInfo
            { cfHash = Hash "zZx4F64Y6MG1YGUuxKDusPLIlVmILO6qaQZymdsmWmk="
            , cfLength = 3
            , cfModTime = Timestamp 42
            , cfVersion = VersionId "100"
            }
          )
        , ( EntryName "a/b/e/f/foo"
          , CloudFile CloudFileInfo
            { cfHash = Hash "9wjg36DLTfAOSUT+NxKJmA0dCZW6bRW8pzZj+LGgN+s="
            , cfLength = 4
            , cfModTime = Timestamp 42
            , cfVersion = VersionId "101"
            }
          )
        ]
        [ UpdateCloudFile
          { faFilename = EntryName "a/b/c/foo"
          , faLocalInfo = LocalFileInfo
            { lfLength = 4
            , lfModTime = Timestamp 42
            }
          }
        ]

    writeFile (root </> "a" </> "b" </> "c" </> "foo") "foo1"
    diff3 <- getChanges'
        ( \(f, i) -> if f == EntryName "a/b/c/foo"
                        then (f, i { lfModTime = Timestamp 1 })
                        else (f, i)
        )
        [ ( EntryName "a/b/c/foo"
          , CloudFile CloudFileInfo
            { cfHash = Hash "B+9p2ru9/sTS5mdIPgWncWKBHpH76aY+p7/UaoXBlwM="
            , cfLength = 4
            , cfModTime = Timestamp 42
            , cfVersion = VersionId "105"
            }
          )
        , ( EntryName "a/b/e/f/foo"
          , CloudFile CloudFileInfo
            { cfHash = Hash "9wjg36DLTfAOSUT+NxKJmA0dCZW6bRW8pzZj+LGgN+s="
            , cfLength = 4
            , cfModTime = Timestamp 42
            , cfVersion = VersionId "101"
            }
          )
        ]
    assertEqual "can't detect file write without len change"
        [ UpdateCloudFile
          { faFilename = EntryName "a/b/c/foo"
          , faLocalInfo = LocalFileInfo
            { lfLength = 4
            , lfModTime = Timestamp 1
            }
          }
        ]
        diff3
    updateDb diff3

    check "can't detect timestamp only update"
        [ ( EntryName "a/b/c/foo"
          , CloudFile CloudFileInfo
            { cfHash = Hash "030RQSMx83MhsKJrqDbkXvlkg5KJ3hjtsSA8o3Vs0bQ="
            , cfLength = 4
            , cfModTime = Timestamp 1
            , cfVersion = VersionId "108"
            }
          )
        , ( EntryName "a/b/e/f/foo"
          , CloudFile CloudFileInfo
            { cfHash = Hash "9wjg36DLTfAOSUT+NxKJmA0dCZW6bRW8pzZj+LGgN+s="
            , cfLength = 4
            , cfModTime = Timestamp 42
            , cfVersion = VersionId "101"
            }
          )
        ]
        [ UpdateCloudMetadata
          { faFilename = EntryName "a/b/c/foo"
          , faLocalInfo = LocalFileInfo
            { lfLength = 4
            , lfModTime = Timestamp 42
            }
          , faExpectedHash = Hash "030RQSMx83MhsKJrqDbkXvlkg5KJ3hjtsSA8o3Vs0bQ="
          }
        ]

    removeFile (root </> "a" </> "b" </> "e" </> "f" </> "foo")
    check "can't detect file removal"
        [ ( EntryName "a/b/c/foo"
          , CloudFile CloudFileInfo
            { cfHash = Hash "030RQSMx83MhsKJrqDbkXvlkg5KJ3hjtsSA8o3Vs0bQ="
            , cfLength = 4
            , cfModTime = Timestamp 42
            , cfVersion = VersionId "108"
            }
          )
        , ( EntryName "a/b/e/f/foo"
          , CloudFile CloudFileInfo
            { cfHash = Hash "9wjg36DLTfAOSUT+NxKJmA0dCZW6bRW8pzZj+LGgN+s="
            , cfLength = 4
            , cfModTime = Timestamp 42
            , cfVersion = VersionId "101"
            }
          )
        ]
        [ DeleteCloudFile
          { faFilename = EntryName "a/b/e/f/foo"
          }
        ]

    check "can't detect server add"
        [ ( EntryName "a/b/c/foo"
          , CloudFile CloudFileInfo
            { cfHash = Hash "030RQSMx83MhsKJrqDbkXvlkg5KJ3hjtsSA8o3Vs0bQ="
            , cfLength = 4
            , cfModTime = Timestamp 42
            , cfVersion = VersionId "108"
            }
          )
        , ( EntryName "a/b/e/buzz"
          , CloudFile CloudFileInfo
            { cfHash = Hash "9wjg36DLTfAOSUT+NxKJmA0dCZW6bRW8pzZj+LGgN+s="
            , cfLength = 4
            , cfModTime = Timestamp 42
            , cfVersion = VersionId "1"
            }
          )
        ]
        [ UpdateLocalFile
          { faFilename = EntryName "a/b/e/buzz"
          , faCloudInfo = CloudFileInfo
            { cfHash = Hash "9wjg36DLTfAOSUT+NxKJmA0dCZW6bRW8pzZj+LGgN+s="
            , cfLength = 4
            , cfModTime = Timestamp 42
            , cfVersion = VersionId "1"
            }
          }
        ]
    writeFile (root </> "a" </> "b" </> "e" </> "buzz") "barr"

    check "can't detect server edit"
        [ ( EntryName "a/b/c/foo"
          , CloudFile CloudFileInfo
            { cfHash = Hash "030RQSMx83MhsKJrqDbkXvlkg5KJ3hjtsSA8o3Vs0bQ="
            , cfLength = 4
            , cfModTime = Timestamp 42
            , cfVersion = VersionId "108"
            }
          )
        , ( EntryName "a/b/e/buzz"
          , CloudFile CloudFileInfo
            { cfHash = Hash "B+9p2ru9/sTS5mdIPgWncWKBHpH76aY+p7/UaoXBlwM="
            , cfLength = 4
            , cfModTime = Timestamp 42
            , cfVersion = VersionId "2"
            }
          )
        ]
        [ UpdateLocalFile
          { faFilename = EntryName "a/b/e/buzz"
          , faCloudInfo = CloudFileInfo
            { cfHash = Hash "B+9p2ru9/sTS5mdIPgWncWKBHpH76aY+p7/UaoXBlwM="
            , cfLength = 4
            , cfModTime = Timestamp 42
            , cfVersion = VersionId "2"
            }
          }
        ]
    writeFile (root </> "a" </> "b" </> "e" </> "buzz") "fooo"

    check "can't detect server metadata change"
        [ ( EntryName "a/b/c/foo"
          , CloudFile CloudFileInfo
            { cfHash = Hash "030RQSMx83MhsKJrqDbkXvlkg5KJ3hjtsSA8o3Vs0bQ="
            , cfLength = 4
            , cfModTime = Timestamp 42
            , cfVersion = VersionId "108"
            }
          )
        , ( EntryName "a/b/e/buzz"
          , CloudFile CloudFileInfo
            { cfHash = Hash "B+9p2ru9/sTS5mdIPgWncWKBHpH76aY+p7/UaoXBlwM="
            , cfLength = 4
            , cfModTime = Timestamp 50
            , cfVersion = VersionId "2"
            }
          )
        ]
        [ UpdateLocalMetadata
          { faFilename = EntryName "a/b/e/buzz"
          , faCloudInfo = CloudFileInfo
            { cfHash = Hash "B+9p2ru9/sTS5mdIPgWncWKBHpH76aY+p7/UaoXBlwM="
            , cfLength = 4
            , cfModTime = Timestamp 50
            , cfVersion = VersionId "2"
            }
          }
        ]

    check "can't detect server delete with marker"
        [ ( EntryName "a/b/c/foo"
          , CloudFile CloudFileInfo
            { cfHash = Hash "030RQSMx83MhsKJrqDbkXvlkg5KJ3hjtsSA8o3Vs0bQ="
            , cfLength = 4
            , cfModTime = Timestamp 42
            , cfVersion = VersionId "108"
            }
          )
        , ( EntryName "a/b/e/buzz"
          , CloudDeleteMarker
          )
        ]
        [ DeleteLocalFile
          { faFilename = EntryName "a/b/e/buzz"
          }
        ]
    removeFile (root </> "a" </> "b" </> "e" </> "buzz")

    check "can't detect server delete with missing record"
        [ ( EntryName "a/b/e/buzz"
          , CloudDeleteMarker
          )
        ]
        [ DeleteLocalFile
          { faFilename = EntryName "a/b/c/foo"
          }
        ]
    removeFile (root </> "a" </> "b" </> "c" </> "foo")

testZipLists3 :: Assertion
testZipLists3 =
    assertEqual "zipLists"
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

testPath2Entry :: Assertion
testPath2Entry = do
    assertEqual "single name" (EntryName "foo") $ path2entry "foo"
    assertEqual "path with directory"
        (EntryName "foo/bar.dat/buzz.txt")
        (path2entry $ joinPath ["foo", "bar.dat", "buzz.txt"])
    assertEqual "multi-slash name"
        (EntryName "foo/bar/buzz.txt")
        (path2entry $ "foo" ++  pathSeparator : pathSeparator : "bar" </> "buzz.txt")

testEntry2Path :: Assertion
testEntry2Path = do
    assertEqual "single name" "foo" (entry2path $ EntryName "foo")
    assertEqual "path with directory"
        (joinPath ["foo", "bar.dat", "buzz.txt"])
        (entry2path $ EntryName "foo/bar.dat/buzz.txt")

testEntryFile :: Assertion
testEntryFile = do
    assertEqual "single name" "foo" (entryFile $ EntryName "foo")
    assertEqual "path with directory"
        "buzz.txt"
        (entryFile $ EntryName "foo/bar.dat/buzz.txt")
