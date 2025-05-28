module Pages.Guestbook.Guestbook where

import IHP.HSX.QQ (hsx)
import Text.Blaze.Html (Html)

import Section (section)

import Data.List (filter)

import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Database.Schema (GuestbookEntry (GuestbookEntry), GuestbookEntryId, Key (GuestbookEntryKey), EntityField (GuestbookEntryId))
import Layout (layout)
import Page (Page, PageSetting (Description, Route))
import Tree (Tree (Tree))
import Database.Database (AdminTable(getRows))
import Database.Persist (Entity (Entity))
import Database.Persist.MySQL (toSqlKey, BackendKey (SqlBackendKey))

type Guestbook = [(Int, Int, String, String, Int)]

toPosix :: Int -> POSIXTime
toPosix n = read (show n ++ "s") :: POSIXTime

prettifyGuestbook :: [Tree (Entity GuestbookEntry)] -> Html
prettifyGuestbook ((Tree (Entity (GuestbookEntryKey (SqlBackendKey id)) (GuestbookEntry timestamp name content parent)) children) : xs) = mconcat [
    section [hsx|
        <h3>{name} said: </h3>
        Posted: <span style="color: #ff0000">{formatTime defaultTimeLocale "%c" $ posixSecondsToUTCTime (toPosix timestamp)}</span>
        <br>
        <div style="background-color: #111111; border: 1px solid #111111; border-radius: 5px; padding: 10px;">
            <table>
                <tr>
                    <th style="background-color: #303030; width: 2px;"></th>
                    <th>
                        <div style="white-space: pre;">
                        {content}
                        </div>
                    </th>
                </tr>
            </table>
        </div>
        {prettifyGuestbook $ children} 
        {guestbookInput (fromIntegral id) True}
        <br><br>
    |], prettifyGuestbook xs
    ]
prettifyGuestbook [] = [hsx||]

guestbookInput :: Int -> Bool -> Html
guestbookInput parent False = [hsx|
    <textarea class="guestbook-text" id={"guestbook-text::"++show parent} type="text"></textarea>
    <br>
    Name: <input id={"guestbook-name::"++show parent} class="guestbook-name" type="text">
    <button id={show parent} onclick="post(this.id)">Post</button>
|]
guestbookInput parent True = [hsx|
    <button id={show parent} onclick="guestbookToggleInput(this.id)">New reply</button>
    <br>
    <div style="display:none;" id={"guestbook-reply::"++show parent}>
        {guestbookInput parent False}
    </div>
|]

guestbookToTree :: [Entity GuestbookEntry] -> GuestbookEntryId -> [Tree (Entity GuestbookEntry)]
guestbookToTree entries targetParent = [Tree (Entity id (GuestbookEntry timestamp name content parent)) $ guestbookToTree entries id | (Entity id (GuestbookEntry timestamp name content parent)) <- entries, (toSqlKey . fromIntegral) parent == targetParent]

getGuestbook :: IO [Tree (Entity GuestbookEntry)]
getGuestbook = do
    entries <- getRows [] [] :: IO [Entity GuestbookEntry]
    return $ guestbookToTree entries ((toSqlKey . read . show) (-1))


page :: IO Html
page = do
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
                    method:"PUT",
                    body: JSON.stringify({
                        timestamp: Math.floor(Date.now() / 1000),
                        name: name,
                        content: text,
                        parentId: Number(id)
                    })
                }).then(response => {
                    if(response.status == 200){
                        window.location.reload()
                    }
                    else response.json().then(data => {
                        alert(response.status + "\n" + data.message)
                    })
                })
            }
        </script>
        <h1>Guestbook</h1>
        Write a message for me :)<br>
        {guestbookInput (-1) False}
        <hr>
        <h2>History</h2>
        {prettifyGuestbook guestbook}
    |]

settings :: [PageSetting]
settings = [
    Route "/guestbook", 
    Description "A Guestbook where you can send me a message"
    ]

guestbook :: Page
guestbook = (settings, const $ layout <$> page)
