{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Network.Simple.TCP (serve, HostPreference(HostAny))
import Network.Socket.ByteString (recv, send)
import Control.Monad (forever, guard)
import Data.ByteString (ByteString, pack)
import qualified Data.ByteString.Char8 as B
import Prelude hiding (concat)
import Text.Megaparsec
    ( parse,
      count,
      (<|>),
      Parsec,
      MonadParsec(try),
      Stream(Tokens) )
import Text.Megaparsec.Byte ( crlf, printChar, string )
import Text.Megaparsec.Byte.Lexer (decimal)
import Data.Void ( Void )
import Data.Text ( toLower, Text )
import Data.Text.Encoding (decodeUtf8)
import Data.Map (Map, insert, findWithDefault, empty)
import Data.Map.Internal.Debug (showTree)
import Control.Concurrent.STM (atomically, newTVarIO, readTVarIO, TVar)
import Control.Concurrent.STM.TVar (modifyTVar)

type Request = ByteString
type Response = ByteString
type Parser = Parsec Void Request
type Message = ByteString
type Key = ByteString
type Value = ByteString
type DB = Map Key Value

data Command = Ping
             | Echo Message
             | Set Key Value
             | Get Key
data ApplicationError = UnknownCommand
data ProgConfigs = ProgConfigs {
    recvBytes :: Int }
data CmdNames = CmdNames {
    pingN :: Text,
    echoN :: Text,
    setN :: Text,
    getN :: Text }
data RedisSpecs = RedisSpecs {
    port :: String,
    pingDefault :: Response,
    unknownCmd :: Response,
    setSuccess :: Response,
    nilString :: Response,
    bulkStringId :: ByteString,
    arrayId :: ByteString,
    simpleStringId :: ByteString }

main :: IO ()
main = do
    putStrLn $ "\r\n>>> Redis server listening on port " ++ port redisSpecs ++ " <<<"
    redisDB <- setupDB
    serve HostAny (port redisSpecs) $ \(socket, _address) -> do
        putStrLn $ "successfully connected client: " ++ show _address
        _ <- forever $ do
            request <- recv socket $ recvBytes progConfigs
            response <- do
                case parseRequest request of
                    Left _ -> return $ unknownCmd redisSpecs
                    Right cmd -> exec cmd redisDB
            _ <- send socket (encodeRESP response)

            -- debug database
            out <- readTVarIO redisDB
            putStrLn $ "\r\n***\r\nRedis DB content:\r\n"++ showTree out ++ "***\r\n"
        putStrLn $ "disconnected client: " ++ show _address

progConfigs :: ProgConfigs
progConfigs = ProgConfigs {
                recvBytes  = 2048 }

cmdNames :: CmdNames
cmdNames = CmdNames {
                pingN = "ping",
                echoN = "echo",
                setN  = "set",
                getN  = "get" }

redisSpecs :: RedisSpecs
redisSpecs = RedisSpecs {
                port           = "6379",
                pingDefault    = "PONG",
                unknownCmd     = "-ERR Unknown Command",
                setSuccess     = "OK",
                nilString      = "(nil)",
                bulkStringId   = "$",
                arrayId        = "*",
                simpleStringId = "+" }

setupDB :: IO (TVar DB)
setupDB = newTVarIO empty

encodeRESP :: Response -> Response
encodeRESP s = B.concat ["+", s, "\r\n"]

exec :: Command -> TVar DB -> IO Response
exec Ping _ = return "PONG"
exec (Echo msg) _ = return msg
exec (Set key value) db = set key value db
exec (Get key) db = get key db

parseRequest :: Request -> Either ApplicationError Command
parseRequest req = case parseResult of
                       Left _    -> Left UnknownCommand
                       Right cmd -> Right cmd
                   where parseResult = parse parseToCommand "" req

parseToCommand :: Parser Command
parseToCommand = try parseEcho
             <|> try parsePing
             <|> try parseSet
             <|> try parseGet

cmpIgnoreCase :: Text -> Text -> Bool
cmpIgnoreCase a b = toLower a == toLower b

-- some tools escape backslashes
crlfAlt :: Parser (Tokens ByteString)
crlfAlt = "\\r\\n" <|> crlf

redisBulkString :: Parser ByteString
redisBulkString = do
    _ <- string $ bulkStringId redisSpecs
    n <- decimal
    guard $ n >= 0
    _ <- crlfAlt
    s <- count n printChar
    return $ pack s

commandCheck :: Text -> Parser (Integer, ByteString)
commandCheck c = do
    _ <- string $ arrayId redisSpecs
    n <- decimal
    guard $ n > 0
    cmd <- crlfAlt *> redisBulkString
    guard $ cmpIgnoreCase (decodeUtf8 cmd) c
    return (n, cmd)

parseEcho :: Parser Command
parseEcho = do
    (n, _) <- commandCheck $ echoN cmdNames
    guard $ n == 2
    message <- crlfAlt *> redisBulkString
    return $ Echo message

parsePing :: Parser Command
parsePing = do
    (n, _) <- commandCheck $ pingN cmdNames
    guard $ n == 1
    return Ping

parseSet :: Parser Command
parseSet = do
    (n, _) <- commandCheck $ setN cmdNames
    guard $ n == 3
    key <- crlfAlt *> redisBulkString
    value <- crlfAlt *> redisBulkString
    return $ Set key value

parseGet :: Parser Command
parseGet = do
    (n, _) <- commandCheck $ getN cmdNames
    guard $ n == 2
    key <- crlfAlt *> redisBulkString
    return $ Get key

set :: Key -> Value -> TVar DB -> IO Response
set key val db = do
    _ <- atomically $ modifyTVar db $ insert key val
    return $ setSuccess redisSpecs

get :: Key -> TVar DB -> IO Response
get key db = findWithDefault (nilString redisSpecs) key <$> readTVarIO db
