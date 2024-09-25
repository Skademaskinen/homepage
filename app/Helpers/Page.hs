module Helpers.Page where
import Network.Wai (Request (pathInfo))
import Text.Blaze.Html (Html)
import Data.Text (unpack)

type Page = ([PageSetting], Request -> IO Html)

data PageSetting    = EmbedText String
                    | EmbedImage String
                    | Description String
                    | Route String

route :: [PageSetting] -> String
route (x:xs) = case x of
    Route r -> r
    _ -> route xs
route [] = "/"

description :: [PageSetting] -> String
description (x:xs) = case x of
    Description d -> d
    _ -> description xs
description [] = ""

embedText :: [PageSetting] -> String
embedText (x:xs) = case x of
    EmbedText t -> t
    _ -> embedText xs
embedText [] = ""

embedImage :: [PageSetting] -> String
embedImage (x:xs) = case x of
    EmbedImage i -> i
    _ -> embedImage xs
embedImage [] = ""


getArgs :: Request -> [String]
getArgs request = map unpack $ pathInfo request