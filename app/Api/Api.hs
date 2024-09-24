{-# LANGUAGE OverloadedStrings #-}

module Api.Api where


import Helpers.Tables (GuestbookEntry(GuestbookEntry, EmptyGuestbook), LeaderboardEntry (EmptyLeaderboard, LeaderboardEntry), Credentials (EmptyCredentials, Credentials))
import Helpers.Database (getVisits, uuidExists, insert, getGuestbook, getConn)
import Helpers.Utils (unpackBS, getDefault)

import IHP.HSX.QQ (hsx)
import Text.Blaze.Html (Html)

import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.UUID.V4 (nextRandom)
import Data.UUID (toString)

import Network.Wai (getRequestBodyChunk, Request)
import Network.HTTP.Types.Status (Status, status404, status200, status400)

import Data.Aeson (encode, decode, Value (String))
import Data.ByteString.Lazy (fromStrict, toStrict)
import Database.SQLite.Simple (query, Only (Only))
import Data.Password.Bcrypt (PasswordCheck(PasswordCheckSuccess, PasswordCheckFail), mkPassword, checkPassword, PasswordHash (PasswordHash))
import Data.Text (pack, unpack)
import Crypto.Random (getRandomBytes)
import Data.Text.Array (Array(ByteArray))
import Text.StringRandom (stringRandomIO)



handleGuestbookEntry :: GuestbookEntry -> IO (Status, String)
handleGuestbookEntry (GuestbookEntry "" _ _) = return (status400, "Error, name cannot be empty")
handleGuestbookEntry (GuestbookEntry _ "" _) = return (status400, "Error, content cannot be empty")
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

handleLogin :: Credentials -> IO (Status, String)
handleLogin (Credentials username password) = do
    let pass = mkPassword $ pack password
    conn <- getConn
    xs <- query conn "SELECT password FROM users WHERE username = ?" (Only username)
    case xs of
        [Only hash] -> case checkPassword pass (PasswordHash $ pack hash) of
            PasswordCheckSuccess -> do
                xs <- query conn "SELECT token FROM valid_tokens where username = ?" (Only username) :: IO [Only String]
                if null xs then do
                    token <- stringRandomIO "[0-9a-zA-Z]{4}-[0-9a-ZA-Z]{10}-[0-9a-zA-Z]{15}"
                    insert "INSERT INTO valid_tokens (token, username) values (?, ?)" (unpack token :: String, username :: String)
                    return (status200, unpack token)
                else do
                    let (Only x) = head xs
                    return (status200, x)
                    where
            PasswordCheckFail -> return (status400, "Wrong username or password")
        _ -> return (status400, "Error, no user exists")
handleLogin _ = return (status400, "Invalid request")

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
api ["admin", "login"] request = do
    body <- getRequestBodyChunk request
    let credentials = getDefault EmptyCredentials (decode (fromStrict body) :: Maybe Credentials)
    handleLogin credentials
api xs request = do
    return (status404, "{\"error\":\"Endpoint does not exist\"}")
