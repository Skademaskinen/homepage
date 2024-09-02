{-# LANGUAGE OverloadedStrings #-}

module Main where

import IHP.HSX.QQ (hsx)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Blaze.Html (Html)

import Data.Text (unpack)
import Data.List (intercalate, find)

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
import Text.Regex (mkRegex, Regex, matchRegex)
import Pages.Pages (findPage)

app :: Request -> (Response -> IO b)  -> IO b
app request respond = do
    let args = "/" ++ intercalate "/" (map unpack $ pathInfo request)
    let page = findPage args
    response <- page request
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

