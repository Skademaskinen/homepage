module Helpers.Utils where

import IHP.HSX.QQ
import Text.Blaze.Html

forEach :: [Int] -> (Int -> Html) -> Html
forEach (x:xs) f = [hsx|
    {f x}
    {forEach xs f}
|]
forEach [] _ = [hsx||]

link_image :: String -> String -> String -> Html 
link_image label image url = [hsx|
    <div class="inline_container">
        <a href={url}>
            <img src={image} width="100" height="100" style="padding: 1px;">
        </a>
        <br>
        {label}
    </div>
|]

items :: [String] -> Html
items values = forEach [1..length values] (\i -> [hsx|
    <th style="width:100px">{values !! (i-1)}</th>
|])

row :: [String] -> Html
row values = [hsx|
    <tr>
        {items values}
    </tr>
|]


