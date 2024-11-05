module Pages.Admin.Admin where

import CodeBlock (codeBlock)
import Data.Text (Text)
import Database.Database (getGuestbookEntries, getLeaderboard, getTokens, getUsers, getVisits, prettyPrintSchema, tokenToUsername, validateToken)
import Database.Schema (GuestbookEntry (GuestbookEntry), Snake (Snake), Token (Token), User (User), Visit (Visit))
import IHP.HSX.QQ (hsx)
import Layout (layout)
import Page (Page, PageSetting (Description, Route), getArgs)
import Text.Blaze.Html (Html)
import Network.Wai (Request)
import State (getStates, loggedIn, accessToken)

panel :: Html
panel = [hsx|
    Here are actions when logged in
|]

login :: Html
login = [hsx|
    Please type login info here
    <br>
    <script>
        function login() {
            var username = document.getElementById("username").value
            var password = document.getElementById("password").value

            fetch("/api/login", {
                method: "POST",
                body: JSON.stringify({
                    username:username,
                    password:password
                })
            }).then(response => {
                if (response.status == 200)
                    response.json().then(json => {
                        setCookie("accessToken="+json.token)
                        window.location.reload()
                    })
            })
        }
    </script>
    <input id="username" type="text">
    <input id="password" type="password">
    <button onclick="login()">Log in</button>
|]

page :: Request -> IO Html
page request = do
    let states = getStates request
    if loggedIn states then do
        let token = accessToken states
        valid <- validateToken token
        if valid then
            return panel
        else
            return login
    else
        return login

settings :: [PageSetting]
settings = [ 
    Route "/admin",
    Description "Database Admin panel"
    ]

admin :: Page
admin = (settings, \req -> do
    let (_ : xs) = getArgs req
    layout <$> page req
    )
