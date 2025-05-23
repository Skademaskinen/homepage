module Calendar where

import Data.Time (UTCTime (utctDay, UTCTime), formatTime, defaultTimeLocale, addDays, Day, getCurrentTime, secondsToDiffTime, dayOfWeek)
import Database.Schema (Event(Event, eventDate, eventResponsible), Member (Member, memberName), EntityField (EventCancelled))
import Database.Database (runDb, AdminTable (getRows, toList, getList, getLast, getAll))
import Database.Persist (PersistStoreWrite(insert), Entity (Entity), PersistQueryRead (selectFirst), selectList, SelectOpt (LimitTo), (==.))

newtype CalendarData = CalendarData [Event]

generateEvent :: Event -> String
generateEvent (Event timestamp responsible cancelled) = unlines [
    "BEGIN:VEVENT",
    "UID:mast3r@skade.dev",
    "DTSTAMP:" ++ formatTime' timestamp,
    "DTSTART:" ++ formatTime' startTime,
    "DTEND:" ++ formatTime' endTime,
    "SUMMARY:Det Ugentilige Møde",
    "DESCRIPTION:"++responsible++" Folker vognen",
    "END:VEVENT"
    ]
    where
        startTime = UTCTime (utctDay timestamp) (secondsToDiffTime $ 14 * 3600 + 30 * 60)
        endTime   = UTCTime (utctDay timestamp) (secondsToDiffTime $ 19 * 3600)

generateCalendar :: [Event] -> String
generateCalendar events = unlines [
    "BEGIN:VCALENDAR",
    "VERSION:2.0",
    "PRODID:-//Folkevognen//NONSGML Event//EN",
    "METHOD:PUBLISH"
    ] ++ unlines (map generateEvent events) ++ unlines [
    "END:VCALENDAR"
    ]

selectMember :: [Member] -> [Event] -> Member
selectMember members events = head $ filter ((==least) . getCount . memberName) members
    where
        getCount :: String -> Int
        getCount name = length $ filter ((==name) . eventResponsible) events
        counts :: [Int]
        counts = map (getCount . memberName) members
        least :: Int
        least = minimum counts

createEvent :: Int -> IO ()
createEvent _ = do
    eventCount <- length <$> (getList [EventCancelled ==. False] [] :: IO [Event])
    day <- if eventCount == 0 
        then
            utctDay <$> getCurrentTime
        else do
            event <- getLast :: IO Event
            return $ (utctDay . eventDate) event
    let startOfWeek = addDays (negate $ fromIntegral (fromEnum (dayOfWeek day) - 2)) day
    let tuesday = addDays 7 startOfWeek
    let targetDate = UTCTime tuesday 0
    members <- getAll :: IO [Member]
    events <- getList [EventCancelled ==. False] []:: IO [Event]
    let name = memberName $ selectMember members events

    runDb . insert $ Event targetDate name False
    return ()

createEvents :: [Int] -> IO ()
createEvents (n:ns) = do
    createEvent n
    createEvents ns
createEvents [] = return ()

formatTime' :: UTCTime -> String
formatTime' = formatTime defaultTimeLocale "%Y%m%dT%H%M%SZ"

