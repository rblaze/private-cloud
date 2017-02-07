{-# Language StandaloneDeriving, DeriveDataTypeable #-}
module Options where

import Paths_private_cloud (version)
import Data.Version (showVersion)
import System.FilePath
import System.Directory
import System.Console.CmdArgs
import System.Log.Logger (Priority(..))

deriving instance Data Priority

data Options = Options
    { root :: FilePath
    , loglevel :: Priority
    , syncInterval :: Word
    , fullSyncInterval :: Word
    , cleanupInterval :: Word
    , patterns :: [String]
    }
    deriving (Show, Data, Typeable)

getOptions :: IO Options
getOptions = do
    defaultRoot <- (</> "PrivateCloud") <$> getHomeDirectory
    let options = Options
            { root = defaultRoot &= typDir
                &= help ("working directory, default " ++ defaultRoot)
            , loglevel = ERROR &= typ "LEVEL" &= help "log level (ERROR)"
            , syncInterval = 1 &= typ "MIN" &= help "sync interval (1m)"
            , fullSyncInterval = 30 &= typ "MIN" &= help "full sync interval (30m)"
            , cleanupInterval = 720 &= typ "MIN" &= help "database cleanup interval (12h)"
            , patterns = [] &= typ "PATTERN" &= help "file masks to ignore"
            }
            &= helpArg [name "h"]
            &= summary ("PrivateCloud " ++ showVersion version)
            &= program "privatecloud"

    cmdArgs options
