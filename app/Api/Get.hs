module Api.Get where
import Api.Types (APIEndpoint, j2s, jsonHeaders, defaultHeaders, APIRoute, messageResponse, redirect, redirectHeaders, getQueryValue)
import Data.Aeson.QQ (aesonQQ)
import Database.Schema (GuestbookEntry(GuestbookEntry), Member (Member), Event (Event), EntityField (VisitTimestamp, EventDate, EventCancelled))
import Database.Database (AdminTable(getAll, getRows, toList, getList), runDb)
import Database.Persist (Entity(Entity), PersistQueryRead (count), (>.), (==.), PersistStoreWrite (insert))
import Api.Post (postMap)
import Api.Put (putMap)
import Network.HTTP.Types (status200, Query, status400, status308)
import Api.Delete (deleteMap)
import Settings (getEditorRoot)
import System.Directory (getDirectoryContents)
import Data.Text (unpack)
import Network.Wai (Request(pathInfo, queryString))
import System.IO (openFile, IOMode (ReadMode), hGetContents)
import Data.Time (getCurrentTime, defaultTimeLocale, formatTime)
import Calendar (generateCalendar, createEvents)
import Data.ByteString (fromStrict)
import Utils (unpackBS)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

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
        let name = getQueryValue "guestbook-name" query
        let text = getQueryValue "guestbook-text" query
        let id = getQueryValue "id" query
        if null name then
            return (status400, "Error, name cannot be empty", defaultHeaders)
        else if null text then
            return (status400, "Error, content cannot be empty", defaultHeaders)
        else do
            now <- getCurrentTime
            runDb $ insert $ GuestbookEntry (read $ formatTime defaultTimeLocale "%s" now) name text (read id :: Int)
            return (status308, redirect "/guestbook", redirectHeaders)
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
    )
    ]
