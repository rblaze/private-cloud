{-# LANGUAGE NamedFieldPuns #-}
module PrivateCloud.FileWatch
    ( FileEvent(..)
    , watchLocalDir
    ) where

import Control.Concurrent.STM
import Control.Monad
import System.FilePath
import System.INotify as I

data FileEvent
    = DirCreated FilePath
    | Deleted FilePath
    | Moved FilePath FilePath
    | FileUpdated FilePath
    | Gone                      -- root is now missing
    deriving Show

events :: [EventVariety]
events =
    [ CloseWrite
    , Move
    , Create
    , Delete
    , OnlyDir
    , NoSymlink
    ]

watchLocalDir :: INotify -> FilePath -> TQueue FileEvent -> IO ()
watchLocalDir notify path queue =
    -- рекурсивно обойти каталоги, добавить на каждый watch
    -- при создании каталога обойти и его
    -- при удалении каталога грохнуть всех детишек
    -- move in и move out обрабатывать как new/delete
    void $ addWatch
        notify
        ([DeleteSelf, MoveSelf] ++ events)
        path
        (handleRootEvent notify path queue)

handleRootEvent :: INotify -> FilePath -> TQueue FileEvent -> Event -> IO ()
handleRootEvent notify path queue event = do
    handleEvent notify path queue event
    -- handle extra events for root dir
    case event of
        I.MovedSelf{} -> putStrLn "moved self"
        I.Unmounted{} -> atomically $ writeTQueue queue $ Gone
        I.DeletedSelf{} -> atomically $ writeTQueue queue $ Gone
        _ -> return ()

handleEvent :: INotify -> FilePath -> TQueue FileEvent -> Event -> IO ()
handleEvent notify path queue event = do
    putStrLn $ (show event) ++ " @ " ++ path
    let fullPath = path </> filePath event
    case event of
        I.Created{isDirectory = True} -> do
            _ <- addWatch notify events fullPath
                    (handleEvent notify fullPath queue)
            atomically $ writeTQueue queue $ DirCreated fullPath
        I.Closed{maybeFilePath = Just p, wasWriteable = True} ->
            atomically $ writeTQueue queue $ FileUpdated (path </> p)
        I.MovedOut{} -> putStrLn $ fullPath ++ " moved out"
        I.MovedIn{} -> putStrLn $ fullPath ++ " moved in"
        I.Deleted{} -> atomically $ writeTQueue queue $ PrivateCloud.FileWatch.Deleted fullPath
        _ -> return ()
