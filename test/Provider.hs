{-# Language TypeFamilies #-}
module Provider where

import Control.Monad.IO.Class
import Data.ByteArray as BA
import System.FilePath.Glob
import qualified Data.Text as T

import PrivateCloud.Cloud.Monad
import PrivateCloud.Provider.Class

data TestCloud = TestCloud

instance CloudProvider TestCloud where
    type ProviderMonad TestCloud = IO
    type ProviderContext TestCloud = ()

    runCloud _ (CloudMonad f) = liftIO f

    newContext _ = return ()

    uploadFile _ = error "uploadFile"
    downloadFile _ _ = error "downloadFile"

    uploadFileInfo _ _ = error "uploadFileInfo"
    uploadFileMetadata _ _ = error "uploadFileMetadata"
    uploadDeleteMarker = error "uploadDeleteMarker"
    getAllServerFiles = error "getAllServerFiles"
    getRecentServerFiles = error "getRecentServerFiles"

    cleanupCloud = error "cleanupCloud"

runTestCloud :: FilePath -> [Pattern] -> (T.Text -> IO (Maybe ScrubbedBytes)) -> PrivateCloud TestCloud a -> IO a
runTestCloud = runPrivateCloud

setupTestCloud :: FilePath -> T.Text -> IO ScrubbedBytes
setupTestCloud root name = do
    initCloudSettings root name
    return BA.empty
