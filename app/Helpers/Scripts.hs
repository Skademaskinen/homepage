module Helpers.Scripts where
import Text.Blaze.Html (Html)
import IHP.HSX.QQ (hsx)
import Helpers.Page (PageSetting(Route), Page)

createStorage :: Html
createStorage = [hsx|
    <script>
        var storage = {}
    </script>
|]

initStorage :: String -> Html
initStorage label = [hsx|
    <script data-label={label}>
        storage[document.currentScript.dataset.label] = {}
    </script>
|]

fetchGet :: String -> String -> (String -> Html) -> Html
fetchGet label url callback = [hsx|
        {initStorage label}
        {callback label}
    <script data-label={label} data-url={url}>
        var label = document.currentScript.dataset.label
        var url = document.currentScript.dataset.url
        fetch(url, {
            method: "GET"
        }).then(response => {
            if(response.status == 200) {
                return response.text().then(storage[label].callback)
            }
        })
    </script>
|]

alertRequestApi :: String -> Html
alertRequestApi url = fetchGet "temp" url callback
    where
        callback _ = [hsx|
            <script>
                storage.temp.callback = text => alert(text)
            </script>
        |]

showText :: String -> String -> Html
showText label url = fetchGet label url callback
    where
        callback label = [hsx|
            <div id={label}></div>
            <script data-label={label}>
                var label = document.currentScript.dataset.label
                storage[label].callback = text =>document.getElementById(label).innerHTML = text
            </script>
        |]

page :: Html
page = [hsx|
    {createStorage}
    {alertRequestApi "/api/hello"}
    {showText "apitext" "/api/guestbook/get"}
|]

settings :: [PageSetting]
settings = [
    Route "/test"
    ]


testPage :: Page
testPage = (settings, const $ return page)
    where