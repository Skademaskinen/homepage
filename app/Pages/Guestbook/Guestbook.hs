module Pages.Guestbook.Guestbook where

import IHP.HSX.QQ (hsx)
import Text.Blaze.Html (Html)

import Helpers.Database (getGuestbook)
import Helpers.Section (section)

import Data.List (filter)

import Data.Time.Format.ISO8601
import Data.Time.Format
import Data.Time.Clock.POSIX

type Guestbook = [(Int, Int, String, String, Int)]

toPosix :: Int -> POSIXTime
toPosix n = read ((show n) ++ "s") :: POSIXTime

prettify_guestbook :: Guestbook -> Html
prettify_guestbook ((id, timestamp, name, content, parent):xs) = mconcat [section [hsx|
    <h3>{name} said: </h3>
    <div style="background-color: #111111; border: 1px solid #111111; border-radius: 5px;">
        id: <span style="color: #ff0000">{id}</span> 
        parent: <span style="color: #ff0000">{parent}</span> 
        timestamp: <span style="color: #ff0000">{formatTime defaultTimeLocale "%c" $ posixSecondsToUTCTime (toPosix timestamp)}</span>
        <br><br>
        {content}
    </div>
    {prettify_guestbook $ children} 
    {guestbook_input id True}
    <br><br>
|], prettify_guestbook rest]
    where
        children :: Guestbook
        children = filter (\(_, _, _, _, childParent) -> childParent == id) xs
        rest :: Guestbook
        rest = filter (\(_, _, _, _, childParent) -> childParent /= id) xs
prettify_guestbook [] = [hsx||]

guestbook_input :: Int -> Bool -> Html
guestbook_input parent False = [hsx|
    <textarea class="guestbook-text" id={"guestbook-text::"++show parent} type="text"></textarea>
    <br>
    Name: <input id={"guestbook-name::"++show parent} class="guestbook-name" type="text">
    <button id={show parent} onclick="post(this.id)">Post</button>
|]
guestbook_input parent True = [hsx|
    <button id={show parent} onclick="guestbookToggleInput(this.id)">New reply</button>
    <br>
    <div style="display:none;" id={"guestbook-reply::"++show parent}>
        {guestbook_input parent False}
    </div>
|]

guestbook :: IO Html
guestbook = do
    guestbook <- getGuestbook
    return [hsx|
        <script>
            function guestbookToggleInput(id) {
                var reply = document.getElementById("guestbook-reply::"+id)
                if(reply.style.display == "none") {
                    reply.style.display = "unset"
                }
                else {
                    reply.style.display = "none"
                }
            }
            function post(id) {
                var text = document.getElementById("guestbook-text::"+id).value
                var name = document.getElementById("guestbook-name::"+id).value
                console.log(id)
                fetch("/api/guestbook/add", {
                    method:"POST",
                    body: JSON.stringify({
                        name: name,
                        content: text,
                        parentId: Number(id)
                    })
                }).then(response => {
                    if(response.status == 200){
                        window.location.reload()
                    }
                })
            }
        </script>
        <h1>Guestbook</h1>
        Write a message for me :)<br>
        {guestbook_input (-1) False}
        <hr>
        <h2>History</h2>
        {prettify_guestbook guestbook}
    |]
    