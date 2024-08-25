{-# LANGUAGE OverloadedStrings #-}

module Main where

import IHP.HSX.QQ
import Text.Blaze.Html.Renderer.String
import Text.Blaze.Html

import Data.Text (unpack)
import Data.List

import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Internal
import Network.HTTP.Types (statusCode, status200, status404)
import Blaze.ByteString.Builder (copyByteString)
import Data.ByteString.UTF8 as BU

import Layout

import Index
import Pages.Contact.Contact
import Pages.Projects.Projects

import Helpers.Database
import Api.Api

page404 :: [String] -> Response
page404 args = responseBuilder status404 [("Content-Type", "text/html")] $ mconcat $ map copyByteString [BU.fromString (renderHtml (layout [hsx|
    <h1>404 - Page not found</h1><br>
    params: {args}
|]))]

serve :: Html -> Response
serve content = responseBuilder status200 [("Content-Type", "text/html")] $ mconcat $ map 
    copyByteString [BU.fromString (renderHtml content)]

serveFile :: String -> Response
serveFile path = responseFile status200 [] path Nothing


handleRequest :: [String] -> Request ->  IO Response
handleRequest ("static":xs) request = do return (serveFile $ intercalate "/" ("static":xs))
handleRequest ("api":args) request = do 
    value <- api args
    return (responseBuilder status200 [("Content-Type", "text/plain")] $ mconcat $ map copyByteString [BU.fromString value])
handleRequest ["contact"] request = do return (serve (layout contact))
handleRequest ("projects":project) request = do return (serve (layout (projects project)))
handleRequest [] request = do return (serve (layout index))
handleRequest x request = do return (page404 x)

log :: Request -> Response -> IO ()
log request (ResponseBuilder status _ _) = do
    let method = show (requestMethod request)
    let path = intercalate "/" (map unpack (pathInfo request))
    let response_status = show (statusCode status)
    putStrLn $ method ++ "\r\t| " ++ path ++ "\r\t\t\t\t\t\t| " ++ response_status
log request (ResponseFile status _ _ _) = do
    let method = show (requestMethod request)
    let path = intercalate "/" (map unpack (pathInfo request))
    let response_status = show (statusCode status)
    putStrLn $ method ++ "\r\t| " ++ path ++ "\r\t\t\t\t\t\t| " ++ response_status
log request x = do
    let method = show (requestMethod request)
    let path = intercalate "/" (map unpack (pathInfo request))
    putStrLn $ method ++ "\r\t| " ++ path
    

app :: Request -> (Response -> IO b)  -> IO b
app request respond = do
    response <- (handleRequest (map unpack (pathInfo request)) request)
    Main.log request response
    respond response

main :: IO ()
main = do
    let port = 8000
    init_db
    putStrLn $ "Listening on " ++ show port
    run port app