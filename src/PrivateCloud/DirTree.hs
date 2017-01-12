module PrivateCloud.DirTree where

import System.Directory.Tree
import System.Posix.Files
import System.Posix.Types

data FileInfo
    = FileInfo
    { fiLength :: FileOffset
    , fiModTime :: EpochTime
    }
    | NotAFile
    deriving (Eq, Show)

diffTree :: DirTree FileInfo -> DirTree FileInfo -> [FilePath]
diffTree prev@Dir{} next@Dir{} = []
diffTree _ _ = error "internal error: invalid tree types in diffTree"

makeTree :: FilePath -> IO (DirTree FileInfo)
makeTree root = do
    _ :/ tree <- flip readDirectoryWith root $ \path -> do
        st <- getFileStatus path
        return $ if isRegularFile st
            then FileInfo { fiLength = fileSize st, fiModTime = modificationTime st }
            else NotAFile
    return tree
