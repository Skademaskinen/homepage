{-# LANGUAGE OverloadedStrings #-}

module Main where

import IHP.HSX.QQ
import Text.Blaze.Html.Renderer.String
import Text.Blaze.Html

import Data.Text (unpack)
import Data.List

import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200, status404)
import Blaze.ByteString.Builder (copyByteString)
import Data.ByteString.UTF8 as BU

import Layout

import Index
import Pages.Contact.Contact
import Pages.Projects.Projects

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


api :: Html
api = [hsx|
    api
|]


handleRequest :: [String] -> Request -> Response
handleRequest ("static":xs) request = serveFile $ intercalate "/" ("static":xs)
handleRequest ("api":args) request = serve (layout api)
handleRequest ["contact"] request = serve (layout contact)
handleRequest ("projects":project) request = serve (layout (projects $ intercalate "/" project))
handleRequest [] request = serve (layout index)
handleRequest x request = page404 x

app :: Request -> (Response -> b)  -> b
app request respond = respond (handleRequest (map unpack (pathInfo request)) request)

main :: IO ()
main = do
    let port = 8080
    putStrLn $ "Listening on " ++ show port
    run port app