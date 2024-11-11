{-# LANGUAGE OverloadedStrings #-}

module Main where

import IHP.HSX.QQ (hsx)
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.String (renderHtml)

import Data.List (find, intercalate)
import Data.Text (unpack, isInfixOf, pack)

import System.Directory (doesFileExist)

import Blaze.ByteString.Builder (copyByteString)
import Data.ByteString.UTF8 (ByteString, fromString)
import Network.HTTP.Types (HeaderName, Query, Status, status200, status404, statusCode)
import Network.Wai (Request (queryString, requestHeaderUserAgent), responseBuilder, responseFile, responseLBS)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Internal (Request, Response (ResponseBuilder, ResponseFile), pathInfo, requestMethod)

import Layout (layout)

import Index (index)
import Pages.Contact.Contact (contact)
import Pages.Guestbook.Guestbook (guestbook)
import Pages.Projects.Projects (projects)
import Pages.Projects.Snake (leaderboard)
import Pages.Sources.Sources (sources)

import Api.Api (api, j2s)
import Control.Concurrent (ThreadId, forkIO)
import Control.Monad (when)
import Data.Aeson (encode)
import Data.Aeson.QQ (aesonQQ)
import Data.List.Split (splitOn)
import Database.Database (doMigration)
import Logger (info, logger, tableify, warning)
import Page (description, embedImage, embedText)
import Pages.Admin.Admin (admin)
import Pages.Pages (findPage)
import Repl (repl)
import Settings (getCliState, getMigrate, getPort)
import System.Environment (getArgs)
import Text.Regex (Regex, matchRegex, mkRegex)
import Utils (unpackBS)
import State (getStates, getCookies)
import Footer (footer)

serve :: Html -> Response
serve content = responseBuilder status200 [("Content-Type", "text/html")] $ copyByteString (fromString (renderHtml content))

autoContentType :: String -> (HeaderName, ByteString)
autoContentType path = ("Content-Type", mime extension)
  where
      extension = last (splitOn "." path)
      mime "js" = "application/javascript"
      mime "png" = "image/png"
      mime "svg" = "image/svg+xml"
      mime "css" = "text/css"
      mime _ = "text/plain"

serveFile :: String -> IO Response
serveFile path = do
    info "Serving file"
    exists <- doesFileExist path
    if exists then do
        info "File exists"
        return $ responseFile status200 [autoContentType path] path Nothing
    else do
        warning "No file found!"
        return $ responseLBS status404 [("Content-Type", "text/json")] $ encode [aesonQQ|{"error":"Error, file not found"}|]

checkUserAgent :: Request -> [String] -> Bool
checkUserAgent request (x:xs) = case requestHeaderUserAgent request of
    (Just useragent) -> not (isInfixOf (pack x) (pack $ unpackBS useragent)) && checkUserAgent request xs
    Nothing -> True
checkUserAgent _ [] = True

app :: Request -> (Response -> IO b) -> IO b
app request respond = do
    let xs = map unpack $ pathInfo request
    let x = if null xs then "" else head xs
    let args = "/" ++ intercalate "/" xs
    print $ getStates request
    print $ getCookies request
    response <- if checkUserAgent request ["Conduwuit", "Synapse"] then do
        if x == "static" then do
            -- If the requested content is a file
            serveFile $ intercalate "/" xs
        else if x == "favicon.ico" then do
            -- If the requested file is the icon file
            serveFile "static/favicon.ico"
        else if x == "api" then do
            -- If the request is to the API
            (status, value, headers) <- api request
            return $ responseBuilder status headers $ copyByteString (fromString value)
        else do
            -- If the content is to the HTML Frontend
            let (settings, page) = findPage args
            result <- page request
            let image = if embedImage settings /= "" then [hsx|
                <meta content={embedImage settings} property="og:image">
            |] else [hsx||]
            let text = if embedText settings /= "" then [hsx|
                <meta content={embedText settings} property="og:title">
            |] else [hsx||]
            let desc = if description settings /= "" then [hsx|
                <meta content={description settings} property="og:description">
            |] else [hsx||]
        
            footer' <- footer request
            return $ serve (mconcat [result, image, text, desc, footer'])
    else return $ serve [hsx|Error, Hacky solution to disallow matrix federation to query here|]

    logger request response
    respond response

main :: IO ()
main = do
    port <- getPort
    info $ "Listening on " ++ show port
    putStrLn $ "+" ++ mconcat (replicate 65 "-") ++ "+"
    putStrLn $ tableify ["METHOD", "STATUS", "PATH"]
    putStrLn $ "+" ++ mconcat (replicate 65 "-") ++ "+"
    migrate <- getMigrate
    if migrate then
        doMigration
    else do
        cliState <- getCliState
        if cliState then do
            forkIO $ run port app
            repl
        else run port app
