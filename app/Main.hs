module Main where

import Control.Concurrent
import Control.Exception.Safe
import Control.Monad
import Data.Time.Clock.POSIX
import Foreign.C.Types
import System.Directory
import System.FilePath
import System.Log.Logger
import System.Posix.Files
import System.Posix.Types
import qualified Data.ByteString.Lazy as BL

import PrivateCloud.Aws
import PrivateCloud.Aws.Cleanup
import PrivateCloud.Aws.S3
import PrivateCloud.Aws.SimpleDb
import PrivateCloud.DirTree
import PrivateCloud.FileInfo
import PrivateCloud.LocalDb
import PrivateCloud.Sync

import Options

mainLoggerName :: String
mainLoggerName = "PrivateCloud"

main :: IO ()
main = do
    options <- getOptions
    config <- defaultCloudInfo

    updateGlobalLogger mainLoggerName (setLevel (loglevel options))

    let rootDir = root options
    noticeM mainLoggerName $ "#START #root " ++ rootDir

    withDatabase (rootDir </> dbName) $ \conn -> do
        noticeM mainLoggerName "#DBOPEN"
        forever $ handleAny
            (\e -> errorM mainLoggerName $ "#EXCEPTION #msg " ++ show e) $
            do
                localFiles <- unrollTreeFiles <$> makeTree rootDir
                dbFiles <- getFileList conn
                localDiff <- getLocalChanges rootDir localFiles dbFiles

                forM_ localDiff $ \(f, i) -> case i of
                    LocalContentChange info -> do
                        body <- BL.readFile (rootDir </> f)
                        version <- uploadFile config f body
                        uploadFileInfo config f
                            CloudFileInfo
                                { cfHash = dfHash info
                                , cfModTime = dfModTime info
                                , cfLength = dfLength info
                                , cfVersion = version
                                }
                        putFileInfo conn f info
                    LocalMetadataChange info -> do
                        uploadFileMetadata config f info
                        putFileInfo conn f info
                    LocalDelete -> do
                        deleteFile config f
                        removeFileInfo config f
                        deleteFileInfo conn f

                newDbFiles <- getFileList conn
                serverFiles <- getServerFiles config
                serverDiff <- getServerChanges newDbFiles serverFiles
                forM_ serverDiff $ \(f, i) -> case i of
                    CloudContentChange info -> do
                        body <- downloadFile config f (cfVersion info)
                        createDirectoryIfMissing True (dropFileName $ rootDir </> f)
                        BL.writeFile (rootDir </> f) body
                        setFileTimes (rootDir </> f) (posix2epoch $ cfModTime info) (posix2epoch $ cfModTime info)
                        putFileInfo conn f
                            DbFileInfo
                                { dfHash = cfHash info
                                , dfLength = cfLength info
                                , dfModTime = cfModTime info
                                }
                    CloudMetadataChange info -> do
                        setFileTimes (rootDir </> f) (posix2epoch $ cfModTime info) (posix2epoch $ cfModTime info)
                        putFileInfo conn f
                            DbFileInfo
                                { dfHash = cfHash info
                                , dfLength = cfLength info
                                , dfModTime = cfModTime info
                                }
                    CloudDelete -> do
                        removeFile (rootDir </> f)
                        deleteFileInfo conn f

                deleteOldVersions config
                threadDelay 10000000
    where
    posix2epoch :: POSIXTime -> EpochTime
    posix2epoch = CTime . round
