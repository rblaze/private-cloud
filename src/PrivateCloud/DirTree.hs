{-# Language NamedFieldPuns, RecordWildCards #-}
module PrivateCloud.DirTree where

import Data.List
import Data.Function
import System.Directory.Tree
import System.FilePath
import System.Posix.Files

import PrivateCloud.FileInfo

type FileList = [(FilePath, LocalFileInfo)]

unrollTreeFiles :: DirTree (Maybe LocalFileInfo) -> FileList
unrollTreeFiles tree = filter (\(f, _) -> f /= dbName) $ go "" tree{name = ""}
    where
    go base File{name, file = Just f} = [(base </> name, f)]
    go base Dir{..} = concatMap (go $ base </> name) contents
    go _ _ = []

sortDirByName :: DirTree a -> DirTree a
sortDirByName = transformDir sortD
    where
    sortD (Dir n cs) = Dir n (sortBy (compare `on` name) cs)
    sortD c = c

makeTree :: FilePath -> IO (DirTree (Maybe LocalFileInfo))
makeTree root = do
    _ :/ tree <- flip readDirectoryWith root $ \path -> do
        st <- getFileStatus path
        return $ if isRegularFile st
            then Just LocalFileInfo
                { lfLength = fromIntegral $ fileSize st
                , lfModTime = modificationTime st 
                }
            else Nothing
    return $ sortDirByName tree
