{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Network.Simple.TCP (serve, HostPreference(HostAny), closeSock)
import Network.Socket.ByteString (recv, send)
import Control.Monad (forever, guard)
import Data.ByteString (ByteString, pack)
import qualified Data.ByteString.Char8 as B
import Prelude hiding (concat)
import Text.Megaparsec
    ( ParseErrorBundle,
      parse,
      count,
      (<|>),
      Parsec,
      MonadParsec(try),
      Stream(Tokens) )
import Text.Megaparsec.Byte ( crlf, printChar )
import Text.Megaparsec.Byte.Lexer (decimal)
import Data.Void ( Void )
import Data.Either (fromRight)
import Data.Text ( toLower, Text )
import Data.Text.Encoding (decodeUtf8)
import Data.Map (fromList, Map, insert, findWithDefault)
import Data.Map.Internal.Debug (showTree)
import Control.Concurrent.STM (atomically, newTVarIO, readTVarIO, TVar)
import Control.Concurrent.STM.TVar (modifyTVar)

type Request = ByteString
type Response = ByteString
type Parser = Parsec Void Request
type Command = TVar DB -> IO Response
type Key = ByteString
type Value = ByteString
type Message = ByteString
type DB = Map Key Value

data Configuration = Configuration {
    port :: String,
    recvBytes :: Int,
    pingDefault :: ByteString,
    setSuccess :: ByteString,
    nilString :: ByteString
}

main :: IO ()
main = do
    putStrLn $ "\r\n>>> Redis server listening on port " ++ port redisConfig ++ " <<<"
    redisDB <- setupDB
    serve HostAny (port redisConfig) $ \(socket, _address) -> do
        putStrLn $ "successfully connected client: " ++ show _address
        _ <- forever $ do
            input <- recv socket $ recvBytes redisConfig
            response <- parseInput input redisDB
            _ <- send socket (encodeRESP response)

            -- debug database
            out <- readTVarIO redisDB
            putStrLn $ "\r\n***\r\nRedis DB content:\r\n"++ showTree out ++ "***\r\n"
        closeSock socket

redisConfig :: Configuration
redisConfig = Configuration "6379" 2048 "PONG" "OK" "(nil)"

setupDB :: IO (TVar DB)
setupDB = newTVarIO $ fromList [("__version__", "1.0.0")]

encodeRESP :: Response -> Response
encodeRESP s = B.concat ["+", s, "\r\n"]

parseInput :: Request -> TVar DB -> IO Response
parseInput req = fromRight err response
    where
        err _ = return "-ERR unknown command"
        response = parseRequest req

parseRequest :: Request
    -> Either (ParseErrorBundle ByteString Void) Command
parseRequest = parse parseInstruction ""

parseInstruction :: Parser Command
parseInstruction = try parseEcho
               <|> try parsePing
               <|> try parseSet
               <|> try parseGet

cmpIgnoreCase :: Text -> Text -> Bool
cmpIgnoreCase a b = toLower a == toLower b

-- some tools escape backslashes
crlfAlt :: Parser (Tokens ByteString)
crlfAlt = "\\r\\n" <|> crlf

redisBulkString :: Parser Response
redisBulkString = do
    _ <- "$"  -- Redis Bulk Strings start with $
    n <- decimal
    guard $ n >= 0
    _ <- crlfAlt
    s <- count n printChar
    return $ pack s

commandCheck :: Text -> Parser (Integer, Response)
commandCheck c = do
    _ <- "*"  -- Redis Arrays start with *
    n <- decimal
    guard $ n > 0
    cmd <- crlfAlt *> redisBulkString
    guard $ cmpIgnoreCase (decodeUtf8 cmd) c
    return (n, cmd)

parseEcho :: Parser Command
parseEcho = do
    (n, _) <- commandCheck "echo"
    guard $ n == 2
    message <- crlfAlt *> redisBulkString
    return $ echo message

parsePing :: Parser Command
parsePing = do
    (n, _) <- commandCheck "ping"
    guard $ n == 1
    return $ ping $ pingDefault redisConfig

parseSet :: Parser Command
parseSet = do
    (n, _) <- commandCheck "set"
    guard $ n == 3
    key <- crlfAlt *> redisBulkString
    value <- crlfAlt *> redisBulkString
    return $ set key value

parseGet :: Parser Command
parseGet = do
    (n, _) <- commandCheck "get"
    guard $ n == 2
    key <- crlfAlt *> redisBulkString
    return $ get key

echo :: Message -> TVar DB -> IO Response
echo x _ = return x

-- here, ping does the same as echo; added to clearly separate the two commands
ping :: Message -> TVar DB -> IO Response
ping x _ = return x

set :: Key -> Value -> TVar DB -> IO Response
set key val db = do
    _ <- atomically $ modifyTVar db $ insert key val
    return $ setSuccess redisConfig

get :: Key -> TVar DB -> IO Response
get key db = do
    let err = nilString redisConfig
    out <- readTVarIO db
    return $ findWithDefault err key out
