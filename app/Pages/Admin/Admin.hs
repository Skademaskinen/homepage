module Pages.Admin.Admin where

import CodeBlock (codeBlock)
import Data.Text (Text, unpack, pack)
import Database.Database (prettyPrintSchema, validateToken, runDb, AdminTable (makeButton, toList, getRows))
import Database.Schema (GuestbookEntry (GuestbookEntry, guestbookEntryTimestamp), Snake (Snake, snakeTimestamp), Token (Token), User (User), Visit (Visit, visitTimestamp), defs, EntityField (UserName, TokenToken))
import IHP.HSX.QQ (hsx)
import Layout (layout)
import Page (Page, PageSetting (Description, Route), getArgs)
import Text.Blaze.Html (Html, preEscapedToHtml)
import Network.Wai (Request (pathInfo))
import State (getStates, loggedIn, accessToken)
import Database.Persist (Entity(Entity, entityVal), selectList, EntityNameDB (unEntityNameDB), getEntityDBName, FieldNameHS (unFieldNameHS), FieldDef (fieldHaskell), getEntityFields, (==.), PersistQueryWrite (deleteWhere))
import Database.Persist.MySQL (rawSql, mkColumns)
import Logger (warning)
import Plot (plotSVG, barSVG)
import Data.List (nub)
import Graphics.Matplotlib (toSvg, bar, onscreen, ylim, (%), ylabel, title, xlabel, setSizeInches, o1, (@@), o2, setParameter, Matplotlib)

panel :: IO Html
panel = do 
    visits <-       getRows [] [] :: IO [Entity Visit]
    guestbook <-    getRows [] [] :: IO [Entity GuestbookEntry]
    snake <-        getRows [] [] :: IO [Entity Snake]
    users <-        getRows [] [] :: IO [Entity User]
    valid_tokens <- getRows [] [] :: IO [Entity Token]
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
        <br>
        <a href="/admin/metrics">Show metrics</a>
    |]
    where
        th x = [hsx|<th class="common-table-element">{x}</th>|]
        row :: (String, [Int]) -> Html
        row (name, values) = [hsx|
            <tr>
                <td class="common-table-element">
                    {name}
                </td>
                {mconcat $ map th values}
                <td><a href={"/admin/browse/"++name}>Browse</a></td>
            </tr>
        |]

aggregatedVisits :: IO ([Int], [Int])
aggregatedVisits = do
    timestamps <- fmap (fromIntegral . visitTimestamp . entityVal) <$> getRows [] []
    let unique = nub $ map (\x -> div x (60*60*24)) timestamps
    return (unique, map (\x -> length $ filter (\y -> div y (60*60*24)==x) timestamps) unique)

aggregatedGuestbook :: IO ([Int], [Int])
aggregatedGuestbook = do
    timestamps <- fmap (fromIntegral . guestbookEntryTimestamp . entityVal) <$> getRows [] []
    let unique = nub $ map (\x -> div x (60*60*24)) timestamps
    return (unique, map (\x -> length $ filter (\y -> div y (60*60*24)==x) timestamps) unique)

aggregatedLeaderboard :: IO ([Int], [Int])
aggregatedLeaderboard = do
    timestamps <- fmap (fromIntegral . snakeTimestamp . entityVal) <$> getRows [] []
    let unique = nub $ map (\x -> div x (60*60*24)) timestamps
    return (unique, map (\x -> length $ filter (\y -> div y (60*60*24)==x) timestamps) unique)

common :: Matplotlib
common = setParameter "savefig.transparent" True %
    setParameter "text.color" ([1,1,1,1] :: [Int]) %
    setSizeInches (12 :: Int) (4 :: Int) %
    xlabel "daysSinceEpoch" %
    ylabel "amount"

maxValue :: [Int] -> Int
maxValue (x:y:xs) | x >= y = maxValue (x:xs)
                  | otherwise = maxValue (y:xs)
maxValue [x] = x
maxValue [] = 10

minValue :: [Int] -> Int
minValue (x:y:xs) | x <= y = minValue (x:xs)
                  | otherwise = minValue (y:xs)
minValue [x] = x
minValue [] = 0

metrics :: IO Html
metrics = do
    visitsPlot <- do 
        (unique, aggregated) <- aggregatedVisits
        svg <- toSvg $
            bar [show x | x <- unique] aggregated %
            ylim (minValue aggregated) (maxValue aggregated) %
            title "Visits" %
            common
        return $ case svg of
            Left x -> x
            Right x -> x

    guestbookPlot <- do
        (unique, aggregated) <- aggregatedGuestbook
        svg <- toSvg $
            bar [show x | x <- unique] aggregated %
            ylim (minValue aggregated) (maxValue aggregated) %
            title "Guestbook" %
            common
        return $ case svg of
            Left x -> x
            Right x -> x

    leaderboardPlot <- do
        (unique, aggregated) <- aggregatedLeaderboard
        svg <- toSvg $
            bar [show x | x <- unique] aggregated %
            ylim (minValue aggregated) (maxValue aggregated) %
            title "Snake Leaderboard" %
            common
        return $ case svg of
            Left x -> x
            Right x -> x

    return [hsx|
        <style>
            .plot {
                background-color: #222222;
            }
        </style>
        Visits over time:<br>
        <div class="plot">
            {preEscapedToHtml visitsPlot}
        </div>
        <hr>
        Guestbook entries over time:<br>
        <div class="plot">
            {preEscapedToHtml guestbookPlot}
        </div>
        <hr>
        Snake leaderboard entries over time:<br>
        <div class="plot">
            {preEscapedToHtml leaderboardPlot}
        </div>
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
        <table class="common-table" style="max-height:700px;">
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
            tableData <- getRows [] []:: IO [Entity Visit]
            let columnNames = getColumnNames "visits"
            return $ zip (map toList tableData) (map makeButton tableData)
        getTableData "guestbook" = do
            tableData <- getRows [] [] :: IO [Entity GuestbookEntry]
            let columnNames = getColumnNames "guestbook"
            return $ zip (map toList tableData) (map makeButton tableData)
        getTableData "snake" = do
            tableData <- getRows [] [] :: IO [Entity Snake]
            let columnNames = getColumnNames "snake"
            return $ zip (map toList tableData) (map makeButton tableData)
        getTableData "users" = do 
            tableData <- getRows [] [] :: IO [Entity User]
            let columnNames = getColumnNames "users"
            return $ zip (map toList tableData) (map makeButton tableData)
        getTableData "valid_tokens" = do
            tableData <- getRows [] [] :: IO [Entity Token]
            let columnNames = getColumnNames "valid_tokens"
            return $ zip (map toList tableData) (map makeButton tableData)
        getTableData _ = do
            return [(["Error!"], [hsx||]), (["No such table"], [hsx||])]

        getColumnNames :: String -> [String]
        getColumnNames name = "id" : map (unpack . unFieldNameHS . fieldHaskell) (getEntityFields $ head $ filter ((==pack name) . unEntityNameDB . getEntityDBName) defs)
        
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
route [_, "metrics"] = metrics
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
                        setCookie("accessToken="+json.token + ";max-age=" + (24*60*60*7) + ";path=/")
                        window.location.reload()
                    })
            })
        }
    </script>
    <input id="username" type="text">
    <input id="password" type="password">
    <button onclick="login()">Log in</button>
|]

logout :: Request -> IO Html
logout request = do 
    let states = getStates request
    if loggedIn states then do
        let token = accessToken states
        valid <- validateToken token
        if valid then do
            runDb $ deleteWhere [TokenToken ==. token]
        else warning "Not a valid token"
    else warning "Not logged in!"

    return [hsx|
        <script>
            deleteCookie("accessToken=")
        </script>
        You have successfully logged out!
    |]

page :: Request -> IO Html
page request = do
    let states = getStates request
    if last (pathInfo request) == "logout" then
        logout request
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
