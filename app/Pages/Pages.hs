module Pages.Pages where
import Network.Wai (Response, Request (pathInfo), responseBuilder, responseFile)
import IHP.HSX.QQ (hsx)
import Text.Blaze.Html (Html)
import Text.Regex (Regex, mkRegex, matchRegex)
import Network.HTTP.Types (status404, status200)
import Blaze.ByteString.Builder (copyByteString)
import Data.ByteString.UTF8 (fromString)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Layout (layout)
import Helpers.Logger (info, warning)
import System.Directory (doesFileExist)
import Data.List (intercalate, find)
import Api.Api (api)
import Pages.Contact.Contact (contact)
import Pages.Sources.Sources (sources)
import Pages.Guestbook.Guestbook (guestbook)
import Pages.Projects.Projects (projects)
import Pages.Projects.Snake (leaderboard)
import Pages.Admin.Admin (admin)
import Index (index)
import Data.Text (unpack)
import Pages.Search.Search (search, Page)

page404 :: [String] -> Html
page404 args = layout [hsx|
    <h1>404 - Page not found</h1><br>
    params: {args}
|]

pages :: [Page]
pages = [
            ("/search", "Search for pages", \req -> do
                let [_, query] = getArgs req
                return $ layout $ search pages $ mkRegex query
            ),
            ("/contact", "My Contact Information", \_ -> return $ layout contact),
            ("/sources", "Sources for this website and my source code", \_ -> return $ layout sources),
            ("/guestbook", "A Guestbook where you can send me a message", \_ -> layout <$> guestbook),
            ("/projects", "List of all projects i have done", \req -> do
                let (_:project) = getArgs req
                return $ layout $ projects project
            ),
            ("/snake-leaderboard", "Leaderboard of my snake project", \_ -> layout <$> leaderboard),
            ("/admin", "Database Admin panel", \req -> do
                let (_:xs) = getArgs req
                layout <$> admin xs
            ),
            ("/", "Main page", \_ -> layout <$> index)
        ]
    where
        getArgs request = map unpack $ pathInfo request

findPage :: String -> (Request -> IO Html)
findPage addr = case find (\(regex, _, _) -> case matchRegex (mkRegex regex) addr of
    Nothing -> False
    _ -> True) pages of -- TODO: make regex
    (Just (_, _, x)) -> x
    Nothing -> \req -> do
        let x = map unpack $ pathInfo req
        return $ page404 x