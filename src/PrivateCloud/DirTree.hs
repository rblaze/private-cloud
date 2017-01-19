{-# Language NamedFieldPuns, RecordWildCards #-}
module PrivateCloud.DirTree where

import Data.List
import Data.Function
import System.Directory.Tree
import System.FilePath
import System.Posix.Files
import System.Posix.Types
import qualified Data.ByteString as BS

data FileInfo = FileInfo
    { fiLength :: FileOffset
    , fiModTime :: EpochTime
    , fiHash :: Maybe BS.ByteString
    }
    deriving (Eq, Show)

type FileList = [(FilePath, FileInfo)]

type FileChangeInfo = (FilePath, Maybe FileInfo, Maybe FileInfo)

unrollTreeFiles :: DirTree (Maybe FileInfo) -> FileList
unrollTreeFiles tree = go "" tree{name = ""}
    where
    go base File{name, file = Just f} = [(base </> name, f)]
    go base Dir{..} = concatMap (go $ base </> name) contents
    go _ _ = []

getChangedFiles :: FileList -> FileList -> [FileChangeInfo]
getChangedFiles [] xs = map (\(f, i) -> (f, Nothing, Just i)) xs
getChangedFiles xs [] = map (\(f, i) -> (f, Just i, Nothing)) xs
getChangedFiles l@((lname, linfo) : ls) r@((rname, rinfo) : rs) =
    case compare lname rname of
        LT -> (lname, Just linfo, Nothing) : getChangedFiles ls r
        GT -> (rname, Nothing, Just rinfo) : getChangedFiles l rs
        EQ -> if isSame linfo rinfo
                then getChangedFiles ls rs
                else (lname, Just linfo, Just rinfo) : getChangedFiles ls rs
    where
    isSame leftInfo rightInfo = (fiLength leftInfo == fiLength rightInfo)
                                    && (fiModTime leftInfo == fiModTime rightInfo)

sortDirByName :: DirTree a -> DirTree a
sortDirByName = transformDir sortD
    where
    sortD (Dir n cs) = Dir n (sortBy (compare `on` name) cs)
    sortD c = c

makeTree :: FilePath -> IO (DirTree (Maybe FileInfo))
makeTree root = do
    _ :/ tree <- flip readDirectoryWith root $ \path -> do
        st <- getFileStatus path
        return $ if isRegularFile st
            then Just FileInfo
                { fiLength = fileSize st
                , fiModTime = modificationTime st 
                , fiHash = Nothing
                }
            else Nothing
    return $ sortDirByName tree
