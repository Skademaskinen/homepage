module Footer where

import IHP.HSX.QQ (hsx)
import Text.Blaze.Html (Html)
import State (getStates, loggedIn)
import Network.Wai (Request)

footer :: Request -> Html
footer request = if loggedIn (getStates request) then [hsx|
    <a href="/admin/logout">Log out</a>
    |
    <a href="/admin">Admin Panel</a>
|] else [hsx||]
