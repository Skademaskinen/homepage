module Logger where

import Data.List (intercalate)
import Data.Text (unpack)
import Network.HTTP.Types (Status (statusCode))
import Network.Wai.Internal (Request, Response (ResponseBuilder, ResponseFile), pathInfo, requestMethod)

import Control.Monad (when)
import Settings (LogLevel (..), getCliState, getLogLevel)
import System.IO (hFlush, stdout)
import Utils (unpackBS)

colorStatus :: Int -> String
colorStatus code
  | code < 300 = "\ESC[38;2;0;255;0m" ++ show code ++ "\ESC[0m"
  | code < 400 = "\ESC[38;2;255;255;0m" ++ show code ++ "\ESC[0m"
  | otherwise = "\ESC[38;2;255;0;0m" ++ show code ++ "\ESC[0m"

getPath :: Request -> String
getPath request = intercalate "/" $ map unpack $ pathInfo request

up :: Int -> String
up 0 = ""
up n = "\ESC[" ++ show n ++ "A"

left :: Int -> String
left 0 = ""
left n = "\ESC[" ++ show n ++ "D"

down :: Int -> String
down 0 = ""
down n = "\ESC[" ++ show n ++ "B"

right :: Int -> String
right 0 = ""
right n = "\ESC[" ++ show n ++ "C"

clearEnd :: String
clearEnd = "\ESC[0J"

clearLine :: String
clearLine = "\ESC[0K"

tableify :: [String] -> String
tableify (x : xs) = "| " ++ x ++ replicate (20 - l) ' ' ++ tableify xs
 where
  l = length x
tableify [] = "|"

info :: String -> IO ()
info input = do
  loglevel <- getLogLevel
  case loglevel of
    Info -> putStrLn input
    _ -> return ()
  hFlush stdout

warning :: String -> IO ()
warning input = do
  loglevel <- getLogLevel
  case loglevel of
    Warning -> putStrLn input
    Info -> putStrLn input
    _ -> return ()
  hFlush stdout

error :: String -> IO ()
error msg = do
  putStrLn msg
  hFlush stdout

logger :: Request -> Response -> IO ()
logger request (ResponseBuilder status _ _) = do
  let method = unpackBS (requestMethod request)
  let path = getPath request
  cliState <- getCliState
  when cliState $ putStr $ "\r" ++ clearEnd
  putStrLn $ tableify [method, show $ statusCode status, path]
  when cliState $ putStr "> "
  hFlush stdout
logger request (ResponseFile status _ _ _) = do
  let method = unpackBS (requestMethod request)
  let path = getPath request
  cliState <- getCliState
  when cliState $ putStr $ "\r" ++ clearEnd
  putStrLn $ tableify [method, show $ statusCode status, path]
  when cliState $ putStr "> "
  hFlush stdout
logger request x = do
  let method = unpackBS (requestMethod request)
  let path = getPath request
  cliState <- getCliState
  when cliState $ putStr $ "\r" ++ clearEnd
  putStrLn $ tableify [method, path]
  when cliState $ putStr "> "
  hFlush stdout
