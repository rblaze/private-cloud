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
    }
    deriving (Show, Data, Typeable)

getOptions :: IO Options
getOptions = do
    defaultRoot <- (</> "PrivateCloud") <$> getHomeDirectory
    let options = Options
            { root = defaultRoot &= typDir
                &= help ("working directory, default " ++ defaultRoot)
            , loglevel = ERROR &= typ "LEVEL" &= help "log level, default ERROR"
            }
            &= helpArg [name "h"]
            &= summary ("PrivateCloud " ++ showVersion version)
            &= program "privatecloud"

    cmdArgs options
