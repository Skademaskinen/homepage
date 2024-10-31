module Pages.Pages where

import Api.Api (api)
import Blaze.ByteString.Builder (copyByteString)
import Data.ByteString.UTF8 (fromString)
import Data.List (find, intercalate)
import Data.Text (unpack)
import IHP.HSX.QQ (hsx)
import Index (index)
import Layout (layout)
import Logger (info, warning)
import Network.HTTP.Types (status200, status404)
import Network.Wai (Request (pathInfo), Response, responseBuilder, responseFile)
import Page
import Pages.Admin.Admin (admin)
import Pages.Contact.Contact (contact)
import Pages.Guestbook.Guestbook (guestbook)
import Pages.Projects.Projects (projects)
import Pages.Projects.Snake (leaderboard)
import Pages.Search.Search (search)
import Pages.Sources.Sources (sources)
import Scripts (testPage)
import System.Directory (doesFileExist)
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Regex (Regex, matchRegex, mkRegex)

page404 :: [String] -> Html
page404 args =
  layout
    [hsx|
    <h1>404 - Page not found</h1><br>
    params: {args}
|]

pages :: [Page]
pages =
  [ search pages
  , admin
  , contact
  , sources
  , guestbook
  , projects
  , leaderboard
  , testPage
  , index
  ]

findPage :: String -> Page
findPage addr = case find
  ( \(settings, _) -> case do
      if route settings == "/" && addr /= "/"
        then Nothing
        else matchRegex (mkRegex (route settings)) addr of
      Nothing -> False
      _ -> True
  )
  pages of
  (Just page) -> page
  Nothing -> ([], \req -> return $ page404 (map unpack $ pathInfo req))
