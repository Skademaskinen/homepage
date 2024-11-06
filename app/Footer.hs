module Footer where

import IHP.HSX.QQ (hsx)
import Text.Blaze.Html (Html)
import State (getStates, loggedIn, accessToken)
import Network.Wai (Request)
import Database.Database (validateToken)

footer :: Request -> IO Html
footer request = if loggedIn (getStates request) then do
    valid <- validateToken (accessToken (getStates request))
    return $ if valid then [hsx|
        <a href="/admin/logout">Log out</a>
        |
        <a href="/admin">Admin Panel</a>
    |] else [hsx||]
    else [hsx||]
