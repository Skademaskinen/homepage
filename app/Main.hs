{-# LANGUAGE OverloadedStrings #-}

module Main where

import IHP.HSX.QQ (hsx)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Blaze.Html (Html)

import Data.Text (unpack)
import Data.List (intercalate)

import System.Directory (doesFileExist)

import Network.Wai (responseBuilder, responseFile, Request (queryString))
import Network.Wai.Handler.Warp (run)
import Network.Wai.Internal (Response(ResponseBuilder, ResponseFile), Request, pathInfo, requestMethod)
import Network.HTTP.Types (statusCode, status200, status404, Status, Query)
import Blaze.ByteString.Builder (copyByteString)
import Data.ByteString.UTF8 (fromString)

import Layout (layout)

import Index (index)
import Pages.Contact.Contact (contact)
import Pages.Projects.Projects (projects)
import Pages.Projects.Snake (leaderboard)
import Pages.Sources.Sources (sources)
import Pages.Guestbook.Guestbook (guestbook)

import Helpers.Database (initDb)
import Helpers.Utils (unpackBS)
import Helpers.Settings (getPort, getCliState)
import Helpers.Logger (logger, tableify, info, warning)
import Api.Api (api)
import Control.Concurrent (forkIO, ThreadId)
import Helpers.Cli (cli)
import System.Environment (getArgs)
import Pages.Admin.Admin (admin)

page404 :: [String] -> Response
page404 args = responseBuilder status404 [("Content-Type", "text/html")] $ copyByteString (fromString (renderHtml (layout [hsx|
    <h1>404 - Page not found</h1><br>
    params: {args}
|])))

serve :: Html -> Response
serve content = responseBuilder status200 [("Content-Type", "text/html")] $ copyByteString (fromString (renderHtml content))

serveFile :: String -> IO Response
serveFile path = do
    info "Serving file"
    exists <- doesFileExist path
    if exists then do
        info "File exists"
        return $ responseFile status200 [] path Nothing
    else do
        warning "No file found!"
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
handleRequest ["snake-leaderboard"] request = serve . layout <$> leaderboard
handleRequest ["favicon.ico"] request = do serveFile "static/favicon.ico"
handleRequest ("admin":xs) request = serve . layout <$> admin xs
handleRequest [] request = serve . layout <$> index
handleRequest x request = return $ page404 x

app :: Request -> (Response -> IO b)  -> IO b
app request respond = do
    response <- handleRequest (map unpack (pathInfo request)) request
    logger request response
    respond response

main :: IO ()
main = do
    port <- getPort
    initDb
    info $ "Listening on " ++ show port
    putStrLn $ "+" ++ mconcat (replicate 65 "-") ++ "+"
    putStrLn $ tableify ["METHOD", "STATUS", "PATH"]
    putStrLn $ "+" ++ mconcat (replicate 65 "-") ++ "+"
    cliState <- getCliState
    if cliState then do
        forkIO $ run port app
        cli
    else run port app

