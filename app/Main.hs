{-# LANGUAGE OverloadedStrings #-}

module Main where

import IHP.HSX.QQ
import Text.Blaze.Html.Renderer.String
import Text.Blaze.Html

import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200)
import Blaze.ByteString.Builder (copyByteString)
import Data.ByteString.UTF8 as BU
import Data.ByteString as BS (readFile)

import Index
import Layout

serve content = responseBuilder status200 [("Content-Type", "text/html")] $ mconcat $ map 
    copyByteString [BU.fromString (renderHtml content)]

app request respond = respond $
    case pathInfo request of
        x -> serve (layout index)

main :: IO ()
main = do
    let port = 8080
    putStrLn $ "Listening on " ++ show port
    run port app
