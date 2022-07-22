{-# LANGUAGE OverloadedStrings #-}
module Main where


import Network.Socket
    ( SocketType(..)
    , Family(..)
    , SocketOption(..)
    , SockAddr(..)
    , socket
    , listen
    , bind
    , accept
    , setSocketOption
    , defaultProtocol
    )



main :: IO ()
main = do
    sock <- socket AF_INET Stream defaultProtocol
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet 6379 0)
    listen sock 5
    _ <- accept sock
    return ()
