{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Network.Simple.TCP (serve, HostPreference(HostAny), recv, send)
import Network.Socket (Socket)
import System.IO (hSetBuffering, stdout, stderr, BufferMode(NoBuffering))

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering

    let port = "6379"
    putStrLn $ "Redis server listening on port " ++ port
    serve HostAny port $ \(socket, address) -> do
        putStrLn $ "successfully connected client: " ++ show address
        handleClient socket

handleClient :: Socket -> IO ()
handleClient socket = do
    msg <- recv socket 1024
    case msg of
        Just _ -> do
            send socket "+PONG\r\n"
            handleClient socket
        Nothing -> return ()
