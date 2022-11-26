{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Network.Simple.TCP (serve, HostPreference(HostAny), closeSock)
import Network.Socket.ByteString (recv, send)
import Control.Monad (forever)


main :: IO ()
main = do
    let port = "6379"
    putStrLn $ "\r\n>>> Redis server listening on port " ++ port ++ " <<<"
    serve HostAny port $ \(socket, _address) -> do
        putStrLn $ "successfully connected client: " ++ show _address
        _ <- forever $ do
            _ <- recv socket 2048
            send socket "+PONG\r\n"
        putStrLn $ "disconnected client: " ++ show _address
