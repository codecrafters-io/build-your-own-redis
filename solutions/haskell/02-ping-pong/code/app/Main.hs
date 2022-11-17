{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Network.Simple.TCP (serve, HostPreference(HostAny), closeSock)
import Network.Socket.ByteString (recv, send)


main :: IO ()
main = do
    let port = "6379"
    putStrLn $ "\r\n>>> Redis server listening on port " ++ port ++ " <<<"
    serve HostAny port $ \(socket, _address) -> do
        putStrLn $ "successfully connected client: " ++ show _address
        _ <- recv socket 2048
        _ <- send socket "+PONG\r\n"
        closeSock socket
