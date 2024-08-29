{-# LANGUAGE OverloadedStrings #-}

module Api.Api where


import Helpers.Tables (GuestbookEntry(GuestbookEntry, EmptyGuestbook), LeaderboardEntry (EmptyLeaderboard, LeaderboardEntry))
import Helpers.Database (getVisits, uuidExists, insert, getGuestbook)
import Helpers.Utils (unpackBS, getDefault)

import IHP.HSX.QQ (hsx)
import Text.Blaze.Html (Html)

import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.UUID.V4 (nextRandom)
import Data.UUID (toString)

import Network.Wai (getRequestBodyChunk, Request)
import Network.HTTP.Types.Status (Status, status404, status200, status400)

import Data.Aeson (encode, decode)
import Data.ByteString.Lazy (fromStrict, toStrict)


handleGuestbookEntry :: GuestbookEntry -> IO (Status, String)
handleGuestbookEntry (GuestbookEntry name content parentId) = do
    time <- fmap round getPOSIXTime :: IO Int
    insert "INSERT INTO guestbook (name, timestamp, content, parentId) values (?, ?, ?, ?)" (name :: String, time :: Int, content :: String, parentId :: Int)
    return (status200, "Success")
handleGuestbookEntry EmptyGuestbook = do
    return (status400, "Error")

handleLeaderboardEntry :: LeaderboardEntry -> IO (Status, String)
handleLeaderboardEntry (LeaderboardEntry name score speed fruits) = do
    time <- fmap round getPOSIXTime :: IO Int
    insert "INSERT INTO snake (name, timestamp, score, speed, fruits) values (?, ?, ?, ?, ?)" (name :: String, time :: Int, score :: Int, speed :: Int, fruits :: Int)
    return (status200, "Success")
handleLeaderboardEntry EmptyLeaderboard = return (status400, "Error")

api :: [String] -> Request -> IO (Status, String)
api ["visits", "new"] request = do
    body <- getRequestBodyChunk request
    result <- uuidExists (unpackBS body)
    if result then do
        time <- fmap round getPOSIXTime :: IO Int
        uuid <- nextRandom
        insert "INSERT INTO visits (timestamp, uuid) values (?, ?)" (time :: Int, toString uuid :: String)
        putStrLn "Inserted into db"
        return (status200, toString uuid)
    else
        return (status200, unpackBS body)
api ["visits", "get"] request = do
    visits <- show . length <$> getVisits
    return (status200, visits)
api ["guestbook", "add"] request = do
    body <- getRequestBodyChunk request
    let entry = getDefault EmptyGuestbook (decode (fromStrict body) :: Maybe GuestbookEntry)
    handleGuestbookEntry entry
api ["guestbook", "get"] request = do
    body <- getRequestBodyChunk request
    entries <- getGuestbook
    return (status200, unpackBS $ toStrict $ encode $ show entries)
api ["snake", "add"] request = do
    body <- getRequestBodyChunk request
    let entry = getDefault EmptyLeaderboard (decode (fromStrict body) :: Maybe LeaderboardEntry)
    handleLeaderboardEntry entry
api xs request = do
    return (status404, "{\"error\":\"Endpoint does not exist\"}")
