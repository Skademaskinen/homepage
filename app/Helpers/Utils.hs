module Helpers.Utils where

import IHP.HSX.QQ
import Text.Blaze.Html

forEach :: [Int] -> (Int -> Html) -> Html
forEach (x:xs) f = [hsx|
    {f x}
    {forEach xs f}
|]
forEach [] _ = [hsx||]