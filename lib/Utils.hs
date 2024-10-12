module Utils where

import IHP.HSX.QQ (hsx)
import Text.Blaze.Html (Html)

import Data.ByteString (ByteString)
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8)

forEach :: [Int] -> (Int -> Html) -> Html
forEach (x:xs) f = mconcat [f x, forEach xs f]
forEach [] _ = [hsx||]

linkImage :: String -> String -> String -> Html
linkImage label image url = [hsx|
    <div class="inline_container">
        <a href={url}>
            <img src={image} width="100" height="100" style="padding: 1px;">
        </a>
        <br>
        {label}
    </div>
|]

items :: [Html] -> Html
items values = mconcat $ map (\value -> [hsx|
    <th style="width:100px">{value}</th>
|]) values

row :: [Html] -> Html
row values = [hsx|
    <tr>
        {items values}
    </tr>
|]

unpackBS :: ByteString -> String
unpackBS = unpack . decodeUtf8

getDefault :: a -> Maybe a -> a
getDefault def a = case a of
    (Just v) -> v
    Nothing -> def