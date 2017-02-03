{-# Language OverloadedStrings, NamedFieldPuns, RecordWildCards #-}
module PrivateCloud.DirTree where

import Data.List
import Data.Function
import System.Directory.Tree
import System.Posix.Files
import qualified Data.Text as T

import PrivateCloud.FileInfo

unrollTreeFiles :: DirTree (Maybe LocalFileInfo) -> LocalFileList
unrollTreeFiles tree = filter (\(f, _) -> f /= dbEntry) $ go (EntryName "") tree{name = ""}
    where
    dbEntry = EntryName (T.pack dbName)
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
    _ :/ tree <- flip readDirectoryWith root $ \path -> do
        st <- getFileStatus path
        return $ if isRegularFile st
            then Just LocalFileInfo
                { lfLength = fromIntegral $ fileSize st
                , lfModTime = realToFrac $ modificationTime st 
                }
            else Nothing
    return $ sortDirByName tree
