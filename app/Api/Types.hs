{-# LANGUAGE OverloadedStrings #-}

module Api.Types where
import Network.HTTP.Types (HeaderName, Status, status400, Query, status308)
import Data.ByteString (ByteString, toStrict, pack)
import Data.Aeson (Value, encode)
import Data.Aeson.QQ (aesonQQ)
import Utils (unpackBS)
import Network.Wai (Request)
import Text.Blaze.Html.Renderer.String (renderHtml)
import IHP.HSX.QQ (hsx)

type Header = (HeaderName, ByteString)
type APIResponse = IO (Status, String, [Header])
type APIEndpoint = (String, Request -> APIResponse)
type APIRoute = (String, [APIEndpoint])

j2s :: Value -> String
j2s = unpackBS . toStrict . encode

defaultHeaders :: [Header]
defaultHeaders = [("Content-Type", "text/plain")]

jsonHeaders :: [Header]
jsonHeaders = [("Content-Type", "application/json")]

yamlHeaders :: [Header]
yamlHeaders = [("Content-Type", "application/yaml")]

messageResponse :: String -> String
messageResponse value = j2s [aesonQQ|{
    "message":#{value}
}|]

redirect :: ByteString -> (Status, String, [Header])
redirect url = (status308, "", [("Content-Type", "text/html"), ("Location", url)])

getQueryValue :: String -> Query -> String
getQueryValue key ((key', Just value):xs) | key == unpackBS key' = unpackBS value
                                          | otherwise   = getQueryValue key xs
getQueryValue _ []                        = undefined

(<!>) :: [APIRoute] -> String -> [APIEndpoint]
((method, value):xs) <!> target | method == target  = value
                                | otherwise         = xs <!> target
[] <!> _ = []

(<!!>) :: [APIEndpoint] -> String -> (Request -> APIResponse)
((regex, response):xs) <!!> target  | regex == target  = response
                                    | otherwise         = xs <!!> target
[] <!!> _ = \r -> return (status400, j2s [aesonQQ|{}|], jsonHeaders)
