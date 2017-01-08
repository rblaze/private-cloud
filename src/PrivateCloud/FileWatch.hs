{-# LANGUAGE NamedFieldPuns #-}
module PrivateCloud.FileWatch
    ( FileEvent(..)
    , watchTree
    ) where

import Control.Concurrent.STM
import Control.Exception.Safe
import Control.Monad
import System.Directory
import System.FilePath
import System.INotify as I

data FileEvent
    = DirCreated FilePath
    | Deleted FilePath
    | Moved FilePath FilePath
    | FileUpdated FilePath
    | Gone                      -- root is now missing
    deriving Show

data ReportDirs
    = Report
    | DontReport
    deriving Eq

events :: [EventVariety]
events =
    [ CloseWrite
    , Move
    , Create
    , Delete
    , OnlyDir
    , NoSymlink
    ]

watchTree :: INotify -> FilePath -> TQueue FileEvent -> IO ()
watchTree notify root queue = do
    _ <- addWatch notify events root $ \event -> do
        handleEvent notify queue root event
        -- handle extra events for root dir
        case event of
            I.Ignored{} -> atomically $ writeTQueue queue $ Gone
            _ -> return ()
    entries <- listDirectory root
    forM_ entries $ \entry -> let nextPath = root </> entry
                               in recurse notify queue DontReport nextPath

recurse :: INotify -> TQueue FileEvent -> ReportDirs -> FilePath -> IO ()
recurse notify queue reportDirs path = do
    putStrLn $ "adding watch " ++ path
    handleIO print $ do
        _ <- addWatch notify events path (handleEvent notify queue path)
        when (reportDirs == Report) $ atomically $ writeTQueue queue $ DirCreated path
        entries <- listDirectory path
        forM_ entries $ \entry -> let nextPath = path </> entry
                                   in recurse notify queue reportDirs nextPath

handleEvent :: INotify -> TQueue FileEvent -> FilePath -> Event -> IO ()
handleEvent notify queue path event = do
    putStrLn $ (show event) ++ " @ " ++ path
    let fullPath = path </> filePath event
    case event of
        I.Created{isDirectory = True} -> do
            -- add watch for new directory
            recurse notify queue Report fullPath
        I.Closed{maybeFilePath = Just p, wasWriteable = True} ->
            atomically $ writeTQueue queue $ FileUpdated (path </> p)
--        I.MovedOut{} -> putStrLn $ fullPath ++ " moved out"
--        I.MovedIn{} -> putStrLn $ fullPath ++ " moved in"
        I.Deleted{} ->
            -- watch removed automatically
            atomically $ writeTQueue queue $ PrivateCloud.FileWatch.Deleted fullPath
        _ -> return ()
