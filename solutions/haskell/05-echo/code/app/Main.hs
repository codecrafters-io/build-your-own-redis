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

type Request = ByteString
type Response = ByteString
type Parser = Parsec Void Request
type Command = IO Response

main :: IO ()
main = do
    let port = "6379"
    putStrLn $ "\r\n>>> Redis server listening on port " ++ port ++ " <<<"
    serve HostAny port $ \(socket, _address) -> do
        putStrLn $ "successfully connected client: " ++ show _address
        _ <- forever $ do
            input <- recv socket 2048
            response <- parseInput input
            send socket (encodeRESP response)
        closeSock socket

encodeRESP :: Response -> Response
encodeRESP s = B.concat ["+", s, "\r\n"]

parseInput :: Request -> IO Response
parseInput req = fromRight err response
    where
        err = return "-ERR unknown command"
        response = parseRequest req

parseRequest :: Request
    -> Either (ParseErrorBundle ByteString Void) Command
parseRequest = parse parseInstruction ""

parseInstruction :: Parser Command
parseInstruction = try parseEcho
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
    return $ echo message

parsePing :: Parser Command
parsePing = do
    (n, _) <- commandCheck "ping"
    guard $ n == 1
    return $ ping "PONG"

echo :: ByteString -> IO Response
echo = return

-- here, ping does the same as echo; added to clearly separate the two commands
ping :: ByteString -> IO Response
ping = return
