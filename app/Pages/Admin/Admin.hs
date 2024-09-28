module Pages.Admin.Admin where
import IHP.HSX.QQ (hsx)
import Text.Blaze.Html (Html)
import Database.SQLite.Simple (query, Only (Only), FromRow (fromRow), field)
import Helpers.Database (getConn, prettyPrintSchema)
import Helpers.CodeBlock (codeBlock)
import Helpers.Page (Page, PageSetting (Route, Description), getArgs)
import Layout (layout)
import Helpers.Tables (GuestbookEntry(GuestbookEntry))
import Data.Text (Text)

validateToken :: String -> IO Bool
validateToken token = do
    conn <- getConn
    result <- query conn "SELECT token FROM valid_tokens" () :: IO [Only String]
    case result of
        [] -> return False
        _ -> return True

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
    conn <- getConn
    rows <- query conn "SELECT * FROM visits" () :: IO [(Int, Int, Text)]
    return [hsx|
        <h2>Visits</h2>
        <table class="common-table">
            {nameRow ["id", "timestamp", "uuid"]}
            {mconcat $ map makeRow rows}
        </table>
    |] 
    where
        makeRow (id, timestamp, uuid) = [hsx|
            <tr class="common-table-row">
                <th class="common-table-element">{id}</th>
                <th class="common-table-element">{timestamp}</th>
                <th class="common-table-element">{uuid}</th>
            </tr>
        |]

guestbookTable :: IO Html
guestbookTable = do
    conn <- getConn
    rows <- query conn "SELECT * FROM guestbook" () :: IO [(Int, Int, Text, Text, Int)]
    return [hsx|
        <h2>Guestbook</h2>
        <table class="common-table">
            {nameRow ["id", "timestamp", "name", "content", "parentId"]}
            {mconcat $ map makeRow rows}
        </table>
    |]
    where
        makeRow (id, timestamp, name, content, parent) = [hsx|
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
    conn <- getConn
    rows <- query conn "SELECT * FROM snake" () :: IO [(Int, Int, Text, Int, Int, Int)]
    return [hsx|
        <h2>Snake Leaderboard</h2>
        <table class="common-table">
            {nameRow ["id", "timestamp", "name", "score", "speed", "fruits"]}
            {mconcat $ map makeRow rows}
        </table>
    |]
    where
        makeRow (id, timestamp, name, score, speed, fruits) = [hsx|
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
    conn <- getConn
    rows <- query conn "SELECT * FROM users" () :: IO [(Int, Text, Text)]
    return [hsx|
        <h2>Users</h2>
        <table class="common-table">
            {nameRow ["id", "username", "password"]}
            {mconcat $ map makeRow rows}
        </table>
    |]
    where
        makeRow (id, username, password) = [hsx|
            <tr class="common-table-row">
                <th class="common-table-element">{id}</th>
                <th class="common-table-element">{username}</th>
                <th class="common-table-element">{password}</th>
            </tr>
        |]

validTokensTable :: IO Html
validTokensTable = do
    conn <- getConn
    rows <- query conn "SELECT * FROM valid_tokens" () :: IO [(Int, Text, Text)]
    return [hsx|
        <h2>Valid Tokens</h2>
        <table class="common-table">
            {nameRow ["id", "token", "username"]}
            {mconcat $ map makeRow rows}
        </table>
    |]
    where
        makeRow (id, token, username) = [hsx|
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
        conn <- getConn
        [Only username] <- query conn "SELECT username FROM valid_tokens WHERE token = ?" (Only token) :: IO [Only String]
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
    if validity then showTable table
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
                }).then(response => response.text().then(text => {
                    if(response.ok) {
                        setCookie("adminToken="+text+";path=/")
                        window.location.href = "/admin/summary/"+text
                    }
                    else {
                        var error_display = document.getElementById("error_display")
                        error_display.innerHTML = text
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
    let (_:xs) = getArgs req
    layout <$> page xs)