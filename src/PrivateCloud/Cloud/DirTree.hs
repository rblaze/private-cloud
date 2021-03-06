{-# Language CPP, OverloadedStrings, NamedFieldPuns, RecordWildCards #-}
module PrivateCloud.Cloud.DirTree
    ( makeTree
    , unrollTreeFiles
    ) where

import Data.List
import Data.Function
import System.Directory.Tree
#ifdef WINBUILD
import System.Directory
import System.Win32.File
#else
import System.Posix.Files
#endif

import PrivateCloud.Provider.Types

unrollTreeFiles :: DirTree (Maybe LocalFileInfo) -> LocalFileList
unrollTreeFiles tree = go (EntryName "") tree{name = ""}
  where
    go base File{name, file = Just f} = [(base <//> path2entry name, f)]
    go base Dir{..} = concatMap (go $ base <//> path2entry name) contents
    go _ _ = []

sortDirByName :: DirTree a -> DirTree a
sortDirByName = transformDir sortD
  where
    sortD (Dir n cs) = Dir n (sortBy (compare `on` name) cs)
    sortD c = c

makeTree :: FilePath -> IO (DirTree (Maybe LocalFileInfo))
makeTree root = do
    _ :/ tree <- readDirectoryWith makeFileInfo root
    pure $ sortDirByName tree

makeFileInfo :: FilePath -> IO (Maybe LocalFileInfo)
#ifdef WINBUILD
makeFileInfo path = do
    isFile <- doesFileExist path
    if isFile
        then do
            mtime <- getModificationTime path
            size <- fadFileSize <$> getFileAttributesExStandard path
            pure $ Just LocalFileInfo
                { lfLength = size
                , lfModTime = utc2ts mtime
                }
        else pure Nothing
#else
makeFileInfo path = do
    st <- getFileStatus path
    pure $ if isRegularFile st
        then Just LocalFileInfo
            { lfLength = fromIntegral $ fileSize st
            , lfModTime = epoch2ts $ modificationTime st 
            }
        else Nothing
#endif
