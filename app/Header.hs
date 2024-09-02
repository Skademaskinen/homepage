module Header where

import IHP.HSX.QQ (hsx)
import Text.Blaze.Html (Html)


makeLinks :: [(String, String)] -> Html
makeLinks [] = [hsx||]
makeLinks [(display,url)] = [hsx|
    <a href={url}>{display}</a> | 
    <input id="search" placeholder="search">
    <script>
        var search = document.getElementById("search")
        search.onkeypress = event => {
            if(event.keyCode == 13) {
                window.location.href = "/search/" + search.value
            }
        }
    </script>
|]
makeLinks ((display, url):xs) = [hsx|
    <a href={url}>{display}</a> |
    {makeLinks xs}
|]

header :: [(String, String)] -> Html
header links = [hsx|
    <div>
        {makeLinks links}
    </div>

|]
