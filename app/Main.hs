{-# LANGUAGE OverloadedStrings #-}

module Main where

import IHP.HSX.QQ (hsx)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Blaze.Html (Html)

import Data.Text (unpack)
import Data.List (intercalate)

import Network.Wai (responseBuilder, responseFile)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Internal (Response(ResponseBuilder, ResponseFile), Request, pathInfo, requestMethod)
import Network.HTTP.Types (statusCode, status200, status404)
import Blaze.ByteString.Builder (copyByteString)
import Data.ByteString.UTF8 (fromString)

import Layout

import Index
import Pages.Contact.Contact
import Pages.Projects.Projects
import Pages.Sources.Sources

import Helpers.Database
import Helpers.Utils
import Api.Api

page404 :: [String] -> Response
page404 args = responseBuilder status404 [("Content-Type", "text/html")] $ mconcat $ map copyByteString [fromString (renderHtml (layout [hsx|
    <h1>404 - Page not found</h1><br>
    params: {args}
|]))]

serve :: Html -> Response
serve content = responseBuilder status200 [("Content-Type", "text/html")] $ mconcat $ map 
    copyByteString [fromString (renderHtml content)]

serveFile :: String -> Response
serveFile path = responseFile status200 [] path Nothing


handleRequest :: [String] -> Request ->  IO Response
handleRequest ("static":xs) request = do return (serveFile $ intercalate "/" ("static":xs))
handleRequest ("api":args) request = do 
    value <- api args request
    return (responseBuilder status200 [("Content-Type", "text/plain")] $ mconcat $ map copyByteString [fromString value])
handleRequest ["contact"] request = do return (serve (layout contact))
handleRequest ["sources"] request = do return (serve (layout sources))
handleRequest ("projects":project) request = do return (serve (layout (projects project)))
handleRequest ["favicon.ico"] request = do return (serveFile "static/favicon.ico")
handleRequest [] request = do return (serve (layout index))
handleRequest x request = do return (page404 x)

logger :: Request -> Response -> IO ()
logger request (ResponseBuilder status _ _) = do
    let method = unpackBS (requestMethod request)
    let path = intercalate "/" (map unpack (pathInfo request))
    let response_status = show (statusCode status)
    putStrLn $ method ++ "\r\t| " ++ path ++ "\r\t\t\t\t\t\t| " ++ response_status
logger request (ResponseFile status _ _ _) = do
    let method = unpackBS (requestMethod request)
    let path = intercalate "/" (map unpack (pathInfo request))
    let response_status = show (statusCode status)
    putStrLn $ method ++ "\r\t| " ++ path ++ "\r\t\t\t\t\t\t| " ++ response_status
logger request x = do
    let method = unpackBS (requestMethod request)
    let path = intercalate "/" (map unpack (pathInfo request))
    putStrLn $ method ++ "\r\t| " ++ path
    

app :: Request -> (Response -> IO b)  -> IO b
app request respond = do
    response <- (handleRequest (map unpack (pathInfo request)) request)
    logger request response
    respond response

main :: IO ()
main = do
    let port = 8000
    init_db
    putStrLn $ "Listening on " ++ show port
    run port app
