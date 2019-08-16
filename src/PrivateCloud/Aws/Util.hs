{-# LANGUAGE OverloadedStrings #-}
module PrivateCloud.Aws.Util where

import Control.Monad.IO.Class
import Data.Text.Buildable
import Data.Text.Format
import Data.Time.Clock.POSIX (POSIXTime)
import Data.UUID
import Data.Word (Word64)
import System.Random
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

printSingle :: Buildable t => t -> T.Text
printSingle = TL.toStrict . format "{}" . Only

printTime :: POSIXTime -> T.Text
printTime time = printSingle $ left 14 '0' (round time :: Word64)

mkUUID :: MonadIO m => m T.Text
mkUUID = do
    uuid <- liftIO randomIO
    pure $ T.pack $ show (uuid :: UUID)
