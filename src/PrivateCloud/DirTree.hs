{-# Language NamedFieldPuns, RecordWildCards #-}
module PrivateCloud.DirTree where

import Data.List
import Data.Function
import Data.Text (Text)
import System.Directory.Tree
import System.FilePath
import System.Posix.Files
import System.Posix.Types

data FileInfo
    = FileInfo
        { fiLength :: FileOffset
        , fiModTime :: EpochTime
        , fiHash :: Maybe Text
        }
    | NotAFile
    deriving (Eq, Show)

type FileList = [(FilePath, FileInfo)]

unrollTreeFiles :: DirTree FileInfo -> FileList
unrollTreeFiles tree = go "" tree{name = ""}
    where
    go base File{name, file = f@FileInfo{}} = [(base </> name, f)]
    go base Dir{..} = concatMap (go $ base </> name) contents
    go _ _ = []

getChangedFiles :: FileList -> FileList -> [FilePath]
getChangedFiles [] xs = map fst xs
getChangedFiles xs [] = map fst xs
getChangedFiles l@(left : ls) r@(right : rs) =
    case compare (fst left) (fst right) of
        LT -> fst left : getChangedFiles ls r
        GT -> fst right : getChangedFiles l rs
        EQ -> if snd left == snd right
                then getChangedFiles ls rs
                else fst left : getChangedFiles ls rs

sortDirByName :: DirTree a -> DirTree a
sortDirByName = transformDir sortD
    where
    sortD (Dir n cs) = Dir n (sortBy (compare `on` name) cs)
    sortD c = c

makeTree :: FilePath -> IO (DirTree FileInfo)
makeTree root = do
    _ :/ tree <- flip readDirectoryWith root $ \path -> do
        st <- getFileStatus path
        return $ if isRegularFile st
            then FileInfo
                { fiLength = fileSize st
                , fiModTime = modificationTime st 
                , fiHash = Nothing
                }
            else NotAFile
    return $ sortDirByName tree
