module Header where

import IHP.HSX.QQ
import Text.Blaze.Html


make_links :: [(String, String)] -> Html
make_links [] = [hsx||]
make_links [(display,url)] = [hsx|
    <a href={url}>{display}</a>
|]
make_links ((display, url):xs) = [hsx|
    <a href={url}>{display}</a> |
    {make_links xs}
|]

header :: [(String, String)] -> Html
header links = [hsx|
    <div>
        {make_links links}
    </div>

|]
