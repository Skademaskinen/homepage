module Pages.Admin.Admin where

import CodeBlock (codeBlock)
import Data.Text (Text, unpack, pack)
import Database.Database (getGuestbookEntries, getLeaderboard, getTokens, getUsers, getVisits, prettyPrintSchema, tokenToUsername, validateToken, runDb, AdminTable (button, toList, getData))
import Database.Schema (GuestbookEntry (GuestbookEntry, guestbookEntryRid), Snake (Snake, snakeRid), Token (Token, tokenRid), User (User, userRid), Visit (Visit, visitRid), defs)
import IHP.HSX.QQ (hsx)
import Layout (layout)
import Page (Page, PageSetting (Description, Route), getArgs)
import Text.Blaze.Html (Html)
import Network.Wai (Request (pathInfo))
import State (getStates, loggedIn, accessToken)
import Database.Persist (Entity(Entity), selectList, EntityNameDB (unEntityNameDB), getEntityDBName, FieldNameHS (unFieldNameHS), FieldDef (fieldHaskell), getEntityFields)
import Database.Persist.MySQL (rawSql, mkColumns)

panel :: IO Html
panel = do 
    visits <- mapUnpack $ runDb (selectList [] []) :: IO [Visit]
    guestbook <- mapUnpack $ runDb (selectList [] []) :: IO [GuestbookEntry]
    snake <- mapUnpack $ runDb (selectList [] []) :: IO [Snake]
    users <- mapUnpack $ runDb (selectList [] []) :: IO [User]
    valid_tokens <- mapUnpack $ runDb (selectList [] []) :: IO [Token]
    return [hsx|
        Here are actions when logged in
        <br>
        Database stats:<br>
        <table class="common-table">
            <tr>
                <th class="common-table-element">name</th>
                <th class="common-table-element">length</th>
                <th class="common-table-element"></th>
            </tr>
            {mconcat $ map row [
                ("visits", [length visits]), 
                ("guestbook", [length guestbook]), 
                ("snake", [length snake]),
                ("users", [length users]),
                ("valid_tokens", [length valid_tokens])
            ]}
        </table>
    |]
    where
        unpackEntity (Entity _ e) = e
        mapUnpack = (map unpackEntity <$>)
        th x = [hsx|<th>{x}</th>|]
        row :: (String, [Int]) -> Html
        row (name, values) = [hsx|
            <tr>
                <th class="common-table-element">
                    {name}
                </th>
                {mconcat $ map th values}
                <th><a href={"/admin/browse/"++name}>Browse</a></th>
            </tr>
        |]

browse :: String -> IO Html
browse table = do 
    tableData <- getTableData table
    let columnNames = getColumnNames table
    return [hsx|
        {table} contains:<br>
        <script>
            function delete_row(id) {
                var table = id.split("::")[0]
                var rid = id.split("::")[1]
                fetch("/api/database/delete", {
                    method:"DELETE",
                    body:JSON.stringify({
                        table:table,
                        id:parseInt(rid)
                    })
                }).then(response => {
                    if (response.status == 200)
                        window.location.reload()
                    else alert("Failed to delete row, status: "+ response.status)
                })
            }
        </script>
        <table class="common-table">
            {row (columnNames, empty)}
            {mconcat $ map row tableData}
        </table><br>

        Insert new row below:
        <div id="new-row">
            
        </div>
    |]
    where
        getTableData :: String -> IO [([String], Html)]
        getTableData "visits" = do
            tableData <- getData [] :: IO [Visit]
            let columnNames = getColumnNames "visits"
            return $ zip (map toList tableData) (map button tableData)
        getTableData "guestbook" = do
            tableData <- getData [] :: IO [GuestbookEntry]
            let columnNames = getColumnNames "guestbook"
            return $ zip (map toList tableData) (map button tableData)
        getTableData "snake" = do
            tableData <- getData [] :: IO [Snake]
            let columnNames = getColumnNames "snake"
            return $ zip (map toList tableData) (map button tableData)
        getTableData "users" = do 
            tableData <- getData [] :: IO [User]
            let columnNames = getColumnNames "users"
            return $ zip (map toList tableData) (map button tableData)
        getTableData "valid_tokens" = do
            tableData <- getData [] :: IO [Token]
            let columnNames = getColumnNames "valid_tokens"
            return $ zip (map toList tableData) (map button tableData)
        getTableData _ = do
            return [(["Error!"], [hsx||]), (["No such table"], [hsx||])]

        getColumnNames :: String -> [String]
        getColumnNames name = map (unpack . unFieldNameHS . fieldHaskell) $ getEntityFields $ head $ filter ((==pack name) . unEntityNameDB . getEntityDBName) defs
        
        row :: ([String], Html) -> Html
        row (xs, button) = [hsx|
            <tr>
                {_row xs}
                <th class="common-table-element">
                    {button}
                </th>
            </tr>
        |]
        _row :: [String] -> Html
        _row (x:xs) = [hsx|
            <th class="common-table-element">
                {x}
            </th>
            {_row xs}
        |]
        _row [] = [hsx||]
        empty = [hsx||]

route :: [String] -> IO Html
route [_, "browse", table] = browse table
route _ = panel

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
                        setCookie("accessToken="+json.token + ";path=/")
                        window.location.reload()
                    })
            })
        }
    </script>
    <input id="username" type="text">
    <input id="password" type="password">
    <button onclick="login()">Log in</button>
|]

logout :: Html
logout = [hsx|
    <script>
        deleteCookie("accessToken=")
    </script>
    You have successfully logged out!
|]

page :: Request -> IO Html
page request = do
    let states = getStates request
    if last (pathInfo request) == "logout" then
        return logout
    else if loggedIn states then do
        let token = accessToken states
        valid <- validateToken token
        if valid then
            route (map unpack $ pathInfo request)
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
