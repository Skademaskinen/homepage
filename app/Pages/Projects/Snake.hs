module Pages.Projects.Snake where

import IHP.HSX.QQ (hsx)
import Text.Blaze.Html (Html)

import Database.Schema (Snake (Snake))
import Layout (layout)
import Page (Page, PageSetting (Description, Route))
import Utils (forEach)
import Database.Database (AdminTable(getData))

tile :: Int -> Html
tile id = [hsx|
    <span class="tile" id={"tile-" ++ (show id)}></span>
|]

tileRow :: Int -> Html
tileRow offset = [hsx|
    <div>
        {forEach [(offset*20)..19+(offset*20)] tile}
    </div>
    <br>
|]

grid :: Html
grid = [hsx|
    {forEach [0..19] tileRow}
|]

fire :: Int -> Html
fire _ = [hsx|
    <img src="/static/projects/fire.gif" width="50%">
|]

snake :: Html
snake = [hsx|
    <link rel="stylesheet" href="/static/projects/snake.css">
    <div style="text-align:center;">
        <div class="snake">
            <h3>Danger Noodle!</h3><br>
            <p>... Some client-side jank, a mix of haskell and javascript</p><br><br>
            <div class="wrapper">
                Controls<br>
                Start game: enter<br>
                Up: W or up<br>
                Left: A or left<br>
                Down: S or down<br>
                Right: D or right<br>
                <input style="width:100px" placeholder="Speed" id="speed">
                <input style="width:100px" placeholder="Fruit count" id="fruits">
                <input style="width:100px" placeholder="Player name" id="player-name">
            </div>
            <div style="display:block ruby;">
                Score: <p id="snake-score">0</p>
            </div>
            {grid}
            <script src="/static/projects/snake.js"></script>
            <div style="line-height:normal">
                <button onclick="keyEvent(37)">&lArr;</button>
                <button onclick="keyEvent(38)">&uArr;</button>
                <button onclick="keyEvent(39)">&rArr;</button>
                <button onclick="keyEvent(40)">&dArr;</button>
                <button onclick="keyEvent(13)">Start</button>
            </div>
        </div>
    </div>
    <hr>
    <h2 style="text-align:center;">Scores</h2>
    <div style="text-align:center;">
        <table id="scoreboard" class="common-table">
            <tr>
                <th class="common-table-element">Timestamp</th>
                <th class="common-table-element">Score</th>
                <th class="common-table-element">Speed</th>
                <th class="common-table-element">Fruits</th>
            </tr>
        </table>
        <div id="snakefire" style="position:fixed; width:100%; left:0; top:100%">
            {forEach [0..1] fire}
        </div>
        <br>
        The online leaderboard is too compilicated atm so it has to be on its own page... <br>
        <a href="/snake-leaderboard">snake leaderboard (WIP)</a>
    </div>
|]

leaderboardField :: String -> Html
leaderboardField value = [hsx|
    <th style="width:100px">{value}</th>
|]

leaderboardEntry :: Snake -> Html
leaderboardEntry (Snake id timestamp name score speed fruits) = [hsx|
    <tr class="common-table-element">
        {mconcat $ map leaderboardField [name, show timestamp, show score, show speed, show fruits]}
    </tr>
|]

page :: IO Html
page = do
    l <- getData [] [] :: IO [Snake]
    return [hsx|
        <table class="common-table">
            <tr>
                <th class="common-table-element">Name</th>
                <th class="common-table-element">Time</th>
                <th class="common-table-element">Score</th>
                <th class="common-table-element">Speed</th>
                <th class="common-table-element">Fruits</th>
            </tr>
            {mconcat $ map leaderboardEntry l}
        </table>
    |]

settings :: [PageSetting]
settings = [
    Route "/snake-leaderboard",
    Description "Leaderboard of my snake project"
    ]

leaderboard :: Page
leaderboard = (settings, const $ layout <$> page)
