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
import Pages.Search.Search (search)

page404 :: [String] -> Response
page404 args = responseBuilder status404 [("Content-Type", "text/html")] $ copyByteString (fromString (renderHtml (layout [hsx|
    <h1>404 - Page not found</h1><br>
    params: {args}
|])))

serve :: Html -> Response
serve content = responseBuilder status200 [("Content-Type", "text/html")] $ copyByteString (fromString (renderHtml content))

serveFile :: String -> IO Response
serveFile path = do
    info "Serving file"
    exists <- doesFileExist path
    if exists then do
        info "File exists"
        return $ responseFile status200 [] path Nothing
    else do
        warning "No file found!"
        return $ responseBuilder status404 [("Content-Type", "text/json")] $ copyByteString "{\"error\":\"Error: file not found!\"}"

pages :: [(String, Request -> IO Response)]
pages = [
            ("/static", \req -> do
                let (_:xs) = getArgs req
                serveFile $ intercalate "/" ("static":xs)
            ),
            ("/api", \req -> do
                let (_:args) = getArgs req
                (status, value) <- api args req
                return $ responseBuilder status [("Content-Type", "text/plain")] $ copyByteString (fromString value)
            ),
            ("/search", \req -> do
                let [_, query] = getArgs req
                return $ serve $ layout $ search pages $ mkRegex query
            ),
            ("/contact", \_ -> return $ serve $ layout contact),
            ("/sources", \_ -> return $ serve $ layout sources),
            ("/guestbook", \_ -> serve . layout <$> guestbook),
            ("/projects", \req -> do
                let (_:project) = getArgs req
                return $ serve $ layout $ projects project
            ),
            ("/snake-leaderboard", \_ -> serve . layout <$> leaderboard),
            ("/favicon.ico", \_ -> serveFile "static/favicon.ico"),
            ("/admin", \req -> do
                let (_:xs) = getArgs req
                serve . layout <$> admin xs
            ),
            ("/", \_ -> serve . layout <$> index)
        ]
    where
        getArgs request = map unpack $ pathInfo request

findPage :: String -> (Request -> IO Response)
findPage addr = case find (\(regex, _) -> case matchRegex (mkRegex regex) addr of
    Nothing -> False
    _ -> True) pages of -- TODO: make regex
    (Just x) -> snd x
    Nothing -> \req -> do
        let x = map unpack $ pathInfo req
        return $ page404 x