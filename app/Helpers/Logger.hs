module Helpers.Logger where

import Network.Wai.Internal (Response(ResponseBuilder, ResponseFile), Request, pathInfo, requestMethod)
import Data.List (intercalate)
import Data.Text (unpack)
import Network.HTTP.Types (Status(statusCode))

import Helpers.Utils (unpackBS)
import Helpers.Globals (LogLevel (..), getLogLevel, getCliState)
import System.IO (hFlush, stdout)
import Control.Monad (when)

colorStatus :: Int -> String
colorStatus code | code < 300 = "\ESC[38;2;0;255;0m"++show code++"\ESC[0m"
                 | code < 400 = "\ESC[38;2;255;255;0m"++show code++"\ESC[0m"
                 | otherwise  = "\ESC[38;2;255;0;0m"++show code++"\ESC[0m"

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
tableify (x:xs) = "| " ++ x ++ left l ++ right 20 ++ tableify xs
    where
        l = length x
tableify [] = "|"


info :: String -> IO ()
info input = do
    loglevel <- getLogLevel
    case loglevel of
        Info -> putStrLn $ "\ESC[38;2;100;100;100m" ++ input ++ "\ESC[0m"
        _ -> return ()

warning :: String -> IO ()
warning input = do
    loglevel <- getLogLevel
    case loglevel of
        Warning -> putStrLn $ "\ESC[38;2;255;255;0m" ++ input ++ "\ESC[0m"
        Info -> putStrLn $ "\ESC[38;2;255;255;0m" ++ input ++ "\ESC[0m"
        _ -> return ()


error :: String -> IO ()
error input = putStrLn $ "\ESC[38;2;255;0;0m" ++ input ++ "\ESC[0m"

logger :: Request -> Response -> IO ()
logger request (ResponseBuilder status _ _) = do
    let method = unpackBS (requestMethod request)
    let path = getPath request
    putStr $ "\r" ++ clearEnd
    putStrLn $ tableify [method, show $ statusCode status, path]
    cliState <- getCliState
    when cliState $ do
        putStr "> "
        hFlush stdout
logger request (ResponseFile status _ _ _) = do
    let method = unpackBS (requestMethod request)
    let path = getPath request
    putStr $ "\r" ++ clearEnd
    putStrLn $ tableify [method, show $ statusCode status, path]
    cliState <- getCliState
    when cliState $ do
        putStr "> "
        hFlush stdout
logger request x = do
    let method = unpackBS (requestMethod request)
    let path = getPath request
    putStr $ "\r" ++ clearEnd
    putStrLn $ tableify [method, path]
    cliState <- getCliState
    when cliState $ do
        putStr "> "
        hFlush stdout
