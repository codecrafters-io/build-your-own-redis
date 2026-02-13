{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Network.Simple.TCP (serve, HostPreference(HostAny), closeSock)
import System.IO (hPutStrLn, hSetBuffering, stdout, stderr, BufferMode(NoBuffering))

main :: IO ()
main = do
    -- Disable output buffering
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering

    -- You can use print statements as follows for debugging, they'll be visible when running tests.
    hPutStrLn stderr "Logs from your program will appear here"

    -- Uncomment the code below to pass the first stage
    --
    -- serve HostAny "6379" $ \(socket, _address) -> do
    --     hPutStrLn stderr "Client connected!"
    --     closeSock socket
