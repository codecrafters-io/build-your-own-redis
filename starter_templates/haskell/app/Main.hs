{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Network.Simple.TCP (serve, HostPreference(HostAny), closeSock)


main :: IO ()
main = do
    -- You can use print statements as follows for debugging, they'll be visible when running tests.
    putStrLn "Logs from your program will appear here"

    -- Uncomment this block to pass stage 1
    -- let port = "6379"
    -- putStrLn $ "Redis server listening on port " ++ port
    -- serve HostAny port $ \(socket, address) -> do
    --     putStrLn $ "successfully connected client: " ++ show address
    --     closeSock socket
