module Pages.Admin.Admin where

import CodeBlock (codeBlock)
import Data.Text (Text)
import Database.Database (getGuestbookEntries, getLeaderboard, getTokens, getUsers, getVisits, prettyPrintSchema, tokenToUsername, validateToken)
import Database.Schema (GuestbookEntry (GuestbookEntry), Snake (Snake), Token (Token), User (User), Visit (Visit))
import IHP.HSX.QQ (hsx)
import Layout (layout)
import Page (Page, PageSetting (Description, Route), getArgs)
import Text.Blaze.Html (Html)

nameRow :: [String] -> Html
nameRow names = [hsx|
    <tr>
        {mconcat $ map element names}
    </tr>
|]
    where
        element name = [hsx|
            <th class="common-table-element" style="color: red;">
                {name}
            </th>
        |]

visitsTable :: IO Html
visitsTable = do
    rows <- getVisits
    return [hsx|
        <h2>Visits</h2>
        <table class="common-table">
            {nameRow ["id", "timestamp", "uuid"]}
            {mconcat $ map makeRow rows}
        </table>
    |]
    where
        makeRow (Visit id timestamp uuid) = [hsx|
            <tr class="common-table-row">
                <th class="common-table-element">{id}</th>
                <th class="common-table-element">{timestamp}</th>
                <th class="common-table-element">{uuid}</th>
            </tr>
        |]

guestbookTable :: IO Html
guestbookTable = do
    rows <- getGuestbookEntries
    return [hsx|
        <h2>Guestbook</h2>
        <table class="common-table">
            {nameRow ["id", "timestamp", "name", "content", "parentId"]}
            {mconcat $ map makeRow rows}
        </table>
    |]
    where
        makeRow (GuestbookEntry id timestamp name content parent) = [hsx|
            <tr class="common-table-row">
                <th class="common-table-element">{id}</th>
                <th class="common-table-element">{timestamp}</th>
                <th class="common-table-element">{name}</th>
                <th class="common-table-element">{content}</th>
                <th class="common-table-element">{parent}</th>
            </tr>
        |]

snakeTable :: IO Html
snakeTable = do
    rows <- getLeaderboard
    return [hsx|
        <h2>Snake Leaderboard</h2>
        <table class="common-table">
            {nameRow ["id", "timestamp", "name", "score", "speed", "fruits"]}
            {mconcat $ map makeRow rows}
        </table>
    |]
    where
        makeRow (Snake id timestamp name score speed fruits) = [hsx|
            <tr class="common-table-row">
                <th class="common-table-element">{id}</th>
                <th class="common-table-element">{timestamp}</th>
                <th class="common-table-element">{name}</th>
                <th class="common-table-element">{score}</th>
                <th class="common-table-element">{speed}</th>
                <th class="common-table-element">{fruits}</th>
            </tr>
        |]

usersTable :: IO Html
usersTable = do
    rows <- getUsers
    return [hsx|
        <h2>Users</h2>
        <table class="common-table">
            {nameRow ["id", "username", "password"]}
            {mconcat $ map makeRow rows}
        </table>
    |]
    where
        makeRow (User id username password) = [hsx|
            <tr class="common-table-row">
                <th class="common-table-element">{id}</th>
                <th class="common-table-element">{username}</th>
                <th class="common-table-element">{password}</th>
            </tr>
        |]

validTokensTable :: IO Html
validTokensTable = do
    rows <- getTokens
    return [hsx|
        <h2>Valid Tokens</h2>
        <table class="common-table">
            {nameRow ["id", "token", "username"]}
            {mconcat $ map makeRow rows}
        </table>
    |]
    where
        makeRow (Token id token username) = [hsx|
            <tr class="common-table-row">
                <th class="common-table-element">{id}</th>
                <th class="common-table-element">{token}</th>
                <th class="common-table-element">{username}</th>
            </tr>
        |]

page :: [String] -> IO Html
page ["summary", token] = do
    validity <- validateToken token
    if validity then do
        username <- tokenToUsername token
        return [hsx|
            <h1>Welcome {username}!</h1>
            Schema:
                {codeBlock "txt" prettyPrintSchema}
                <h2>Database tables</h2>
                <a href={"/admin/dump/visits/"++token}>Visits</a><br>
                <a href={"/admin/dump/guestbook/"++token}>Guestbook</a><br>
                <a href={"/admin/dump/snake/"++token}>Snake</a><br>
                <a href={"/admin/dump/users/"++token}>Users</a><br>
                <a href={"/admin/dump/tokens/"++token}>Valid tokens</a><br>
                <a href={"/admin/dump/all/"++token}>All</a>
        |]
    else
        page []
page ["dump", table, token] = do
    validity <- validateToken token
    if validity then 
        showTable table
    else
        page []
    where
        showTable "visits" = visitsTable
        showTable "guestbook" = guestbookTable
        showTable "snake" = snakeTable
        showTable "users" = usersTable
        showTable "tokens" = validTokensTable
        showTable "all" = mconcat [visitsTable, guestbookTable, snakeTable, usersTable, validTokensTable]
        showTable _ = [hsx||]
page x = do
    print x
    return [hsx|
        <script>
            function login() {
                var username = document.getElementById("username").value
                var password = document.getElementById("password").value
                fetch("/api/admin/login", {
                    method: "post",
                    body: JSON.stringify({
                        username:username,
                        password:password
                    })
                }).then(response => response.json().then(data => {
                    if(response.ok) {
                        setCookie("adminToken="+data.token+";path=/")
                        window.location.href = "/admin/summary/"+data.token
                    }
                    else {
                        var error_display = document.getElementById("error_display")
                        error_display.innerHTML = data.message
                    }
                }))
            }
        </script>

        <h1>Admin panel login</h1>
        Manages DB and other stuff...

        <input placeholder="Username" class="admin-input" id="username" type="text">

        <input placeholder="Password" class="admin-input" id="password" type="password">

        <button onclick="login()">Login</button>

        <p id="error_display"></p>
    |]

settings :: [PageSetting]
settings = [ 
    Route "/admin",
    Description "Database Admin panel"
    ]

admin :: Page
admin = (settings, \req -> do
    let (_ : xs) = getArgs req
    layout <$> page xs
    )
