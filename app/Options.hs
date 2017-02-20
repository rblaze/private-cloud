{-# Language StandaloneDeriving, DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
module Options where

import Paths_private_cloud (version)
import Data.Version (showVersion)
import System.FilePath
import System.Directory
import System.Console.CmdArgs
import System.Log.Logger (Priority(..))

deriving instance Data Priority

data Options
    = Run
        { root :: FilePath
        , loglevel :: Priority
        , accountId :: String -- TODO remove
        , syncInterval :: Word
        , fullSyncInterval :: Word
        , cleanupInterval :: Word
        , exclPatterns :: [String]
        }
    | Create
        { root :: FilePath
        , loglevel :: Priority
        , accountId :: String
        , adminKeyId :: String
        , adminSecretKey :: String
        }
    | Connect
        { root :: FilePath
        , loglevel :: Priority
        , accountId :: String
        , clientKeyId :: String
        , clientSecretKey :: String
        }
    deriving (Show, Data, Typeable)

getOptions :: IO Options
getOptions = do
    defaultRoot <- (</> "PrivateCloud") <$> getHomeDirectory
    let run = Run
            { loglevel = ERROR &= typ "LEVEL" &= help "log level (ERROR)"
            , root = defaultRoot &= typDir
                &= help ("working directory (" ++ defaultRoot ++ ")")
            , accountId = "" &= typ "STRING" &= help "account id (saved)"
            , syncInterval = 1 &= typ "MIN" &= help "sync interval (1m)"
            , fullSyncInterval = 30 &= typ "MIN" &= help "full sync interval (30m)"
            , cleanupInterval = 720 &= typ "MIN" &= help "database cleanup interval (12h)"
            , exclPatterns = [".*"] &= typ "PATTERN" &= help "file masks to ignore (.*)"
            }
            &= help "Stay running and sync files with cloud"

    let create = Create
            { root = defaultRoot &= typDir
                &= help ("working directory (" ++ defaultRoot ++ ")")
            , accountId = "" &= name "a" &= typ "STRING" &= help "account id (generate random)"
            , adminKeyId = "" &= typ "STRING" &= help "keyid with AWS admin rights (ask)"
            , adminSecretKey = "" &= typ "STRING" &= help "secret key with AWS admin rights (ask)"
            }
            &= help "Create new cloud"

    let connect = Connect
            { root = defaultRoot &= typDir
                &= help ("working directory (" ++ defaultRoot ++ ")")
            , accountId = "" &= typ "STRING" &= help "account id (required)"
            , clientKeyId = "" &= typ "STRING" &= help "user keyid (ask)"
            , clientSecretKey = "" &= typ "STRING" &= help "user key (ask)"
            }
            &= help "Connect to existing cloud"

    let progArgs = modes [run &= auto, create, connect]
            &= helpArg [name "h"]
            &= summary ("PrivateCloud " ++ showVersion version)
            &= program "privatecloud"

    cmdArgs progArgs
