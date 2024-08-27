{-# LANGUAGE OverloadedStrings #-}

module Main where

import IHP.HSX.QQ (hsx)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Blaze.Html (Html)

import Data.Text (unpack)
import Data.List (intercalate)

import System.Directory (doesFileExist)

import Network.Wai (responseBuilder, responseFile)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Internal (Response(ResponseBuilder, ResponseFile), Request, pathInfo, requestMethod)
import Network.HTTP.Types (statusCode, status200, status404, Status)
import Blaze.ByteString.Builder (copyByteString)
import Data.ByteString.UTF8 (fromString)

import Layout (layout)

import Index (index)
import Pages.Contact.Contact (contact)
import Pages.Projects.Projects (projects)
import Pages.Sources.Sources (sources)
import Pages.Guestbook.Guestbook (guestbook)

import Helpers.Database (initDb)
import Helpers.Utils (unpackBS)
import Helpers.Globals (getPort)
import Api.Api (api)

page404 :: [String] -> Response
page404 args = responseBuilder status404 [("Content-Type", "text/html")] $ copyByteString (fromString (renderHtml (layout [hsx|
    <h1>404 - Page not found</h1><br>
    params: {args}
|])))

serve :: Html -> Response
serve content = responseBuilder status200 [("Content-Type", "text/html")] $ copyByteString (fromString (renderHtml content))

serveFile :: String -> IO Response
serveFile path = do
    exists <- doesFileExist path
    if exists then
        return $ responseFile status200 [] path Nothing
    else
        return $ responseBuilder status404 [("Content-Type", "text/json")] $ copyByteString "{\"error\":\"Error: file not found!\"}"


handleRequest :: [String] -> Request ->  IO Response
handleRequest ("static":xs) request = serveFile $ intercalate "/" ("static":xs)
handleRequest ("api":args) request = do
    (status, value) <- api args request
    return $ responseBuilder status [("Content-Type", "text/plain")] $ copyByteString (fromString value)
handleRequest ["contact"] request = return $ serve (layout contact)
handleRequest ["sources"] request = return $ serve (layout sources)
handleRequest ["guestbook"] request = serve . layout <$> guestbook
handleRequest ("projects":project) request = return $ serve (layout (projects project))
handleRequest ["favicon.ico"] request = do serveFile "static/favicon.ico"
handleRequest [] request = return $ serve (layout index)
handleRequest x request = return $ page404 x

colorStatus :: Int -> String
colorStatus code | code < 300 = "\ESC[38;2;0;255;0m"++show code++"\ESC[0m"
                 | code < 400 = "\ESC[38;2;255;255;0m"++show code++"\ESC[0m"
                 | otherwise  = "\ESC[38;2;255;0;0m"++show code++"\ESC[0m"

logger :: Request -> Response -> IO ()
logger request (ResponseBuilder status _ _) = do
    let method = unpackBS (requestMethod request)
    let path = intercalate "/" (map unpack (pathInfo request))
    putStrLn $ method ++ "\r\t| " ++ path ++ "\r\t\t\t\t\t\t| " ++ colorStatus (statusCode status)
logger request (ResponseFile status _ _ _) = do
    let method = unpackBS (requestMethod request)
    let path = intercalate "/" (map unpack (pathInfo request))
    putStrLn $ method ++ "\r\t| " ++ path ++ "\r\t\t\t\t\t\t| " ++ colorStatus (statusCode status)
logger request x = do
    let method = unpackBS (requestMethod request)
    let path = intercalate "/" (map unpack (pathInfo request))
    putStrLn $ method ++ "\r\t| " ++ path


app :: Request -> (Response -> IO b)  -> IO b
app request respond = do
    response <- handleRequest (map unpack (pathInfo request)) request
    logger request response
    respond response

main :: IO ()
main = do
    port <- getPort
    initDb
    putStrLn $ "Listening on " ++ show port
    run port app
