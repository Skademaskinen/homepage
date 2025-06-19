module Api.Get where
import Api.Types (APIEndpoint, j2s, jsonHeaders, defaultHeaders, APIRoute, redirect, getQueryValue, yamlHeaders)
import Data.Aeson.QQ (aesonQQ)
import Database.Schema (GuestbookEntry(GuestbookEntry), Member (Member), Event (Event), EntityField (VisitTimestamp, EventDate, EventCancelled))
import Database.Database (AdminTable(getAll, getRows, toList, getList), runDb)
import Database.Persist (Entity(Entity), PersistQueryRead (count), (>.), (==.), PersistStoreWrite (insert))
import Api.Post (postMap)
import Api.Put (putMap)
import Network.HTTP.Types (status200, status400)
import Api.Delete (deleteMap)
import Settings (getEditorRoot)
import System.Directory (getDirectoryContents)
import Data.Text (unpack)
import Network.Wai (Request(pathInfo, queryString))
import System.IO (openFile, IOMode (ReadMode), hGetContents)
import Data.Time (getCurrentTime, defaultTimeLocale, formatTime)
import Calendar (generateCalendar, createEvents)
import P10 (cleanup, migrate, prerequisites, autoscaler)

getMap :: [APIRoute] -> [APIEndpoint]
getMap apiMap = [
    ("^(/|)$", \_ -> do
        let apiData = map (\(method, routes) -> [aesonQQ|{
            "method":#{method},
            "routes":#{map fst routes}
        }|]) apiMap
        return (status200, j2s [aesonQQ|#{apiData}|], jsonHeaders)
    ),
    ("^/visits/get(/|)$", \_ -> do
        visits <- runDb $ count [VisitTimestamp >. 0]
        return (status200, j2s [aesonQQ|{"visits":#{visits}}|], jsonHeaders)
    ),
    ("^/guestbook/get(/|)$", \_ -> do
        entries <- getRows [] [] :: IO [Entity GuestbookEntry]
        let l = map toList entries
        return (status200, j2s [aesonQQ|{"entries":#{l}}|], jsonHeaders)
    ),
    ("^/guestbook/add?*$", \r -> do
        let query = queryString r
        let name = getQueryValue "name" query
        let content = getQueryValue "content" query
        let id = read $ getQueryValue "id" query :: Int
        case (name, content, id) of
            ("", _, _) -> return (status400, "Error, name cannot be empty", defaultHeaders)
            (_, "", _) -> return (status400, "Error, content cannot be empty", defaultHeaders)
            (name, content, id) -> do
                now <- read . formatTime defaultTimeLocale "%s" <$> getCurrentTime
                runDb $ insert $ GuestbookEntry now name content id
                return $ redirect "/guestbook"
    ),
    ("^/editor/sidebar(/|)$", \_ -> do
        editor_root <- getEditorRoot
        files <- getDirectoryContents editor_root
        return (status200, j2s [aesonQQ|#{files}|], jsonHeaders)
    ),
    ("^/editor/content/.*(/|)$", \r -> do
        let filename = unpack $ last $ pathInfo r
        editor_root <- getEditorRoot
        handle <- openFile (editor_root ++ "/" ++ filename) ReadMode
        contents  <- hGetContents handle
        return (status200, contents, defaultHeaders)
    ),
    ("^/folkevognen.ics$", \r -> do
        members <- getAll :: IO [Member]
        today <- getCurrentTime
        futureEvents <- getList [EventDate >. today, EventCancelled ==. False] [] :: IO [Event]
        if length futureEvents < 8 then do
            let missing = 8 - length futureEvents
            createEvents [1..missing]
        else
            putStrLn "No new events need to be created"
        futureEvents <- getList [EventDate >. today, EventCancelled ==. False] [] :: IO [Event]
        let calendar = generateCalendar futureEvents
        return (status200, calendar, [("Content-Type", "text/calendar")])
    ),
    ("^/p10/k8s/cleanup.sh", \r -> do
        return (status200, cleanup, defaultHeaders)
    ),
    ("^/p10/k8s/migrate.sh", \r -> do
        return (status200, migrate, defaultHeaders)
    ),
    ("^/p10/k8s/prerequisites.yml", \r -> do
        return (status200, prerequisites, yamlHeaders)
    ),
    ("^/p10/k8s/autoscaler.yml", \r -> do
        return (status200, autoscaler, yamlHeaders)
    )
    ]
