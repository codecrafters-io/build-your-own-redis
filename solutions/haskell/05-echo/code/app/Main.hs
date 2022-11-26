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
import Text.Megaparsec.Byte ( crlf, printChar )
import Text.Megaparsec.Byte.Lexer (decimal)
import Data.Void ( Void )
import Data.Either (fromRight)
import Data.Text ( toLower, Text )
import Data.Text.Encoding (decodeUtf8)

type Request = ByteString
type Response = ByteString
type Parser = Parsec Void Request
type Message = ByteString
data Command = Ping
             | Echo Message
             | Error ApplicationError
data ApplicationError = UnknownCommand

main :: IO ()
main = do
    let port = "6379"
    putStrLn $ "\r\n>>> Redis server listening on port " ++ port ++ " <<<"
    serve HostAny port $ \(socket, _address) -> do
        putStrLn $ "successfully connected client: " ++ show _address
        _ <- forever $ do
            request <- recv socket 2048
            response <- exec $ parseRequest request
            send socket (encodeRESP response)
        putStrLn $ "disconnected client: " ++ show _address

encodeRESP :: Response -> Response
encodeRESP s = B.concat ["+", s, "\r\n"]

exec :: Command -> IO Response
exec Ping = return "PONG"
exec (Echo msg) = return msg
exec (Error UnknownCommand) = return "-ERR Unknown Command"

parseRequest :: Request -> Command
parseRequest req = fromRight err response
    where
        err = Error UnknownCommand
        response = parse parseToCommand "" req

parseToCommand :: Parser Command
parseToCommand = try parseEcho
             <|> try parsePing

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
    return $ Echo message

parsePing :: Parser Command
parsePing = do
    (n, _) <- commandCheck "ping"
    guard $ n == 1
    return Ping
