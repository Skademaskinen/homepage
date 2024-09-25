module Pages.Admin.Admin where
import IHP.HSX.QQ (hsx)
import Text.Blaze.Html (Html)
import Database.SQLite.Simple (query, Only (Only))
import Helpers.Database (getConn, prettyPrintSchema)
import Helpers.CodeBlock (codeBlock)
import Helpers.Page (Page, PageSetting (Route, Description), getArgs)
import Layout (layout)

validateToken :: String -> IO Bool
validateToken token = do
    conn <- getConn
    result <- query conn "SELECT token FROM valid_tokens" () :: IO [Only String]
    case result of
        [] -> return False
        _ -> return True

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
        |]
    else
        page []
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

        <input placeholder="Username" class="admin-input" id="username">

        <input placeholder="Password" class="admin-input" id="password">

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