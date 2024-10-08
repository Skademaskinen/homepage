{-# LANGUAGE OverloadedStrings #-}

module Api.Api where


import qualified Helpers.Tables as T (GuestbookEntry (GuestbookEntry, EmptyGuestbook), LeaderboardEntry (EmptyLeaderboard, LeaderboardEntry), Credentials (EmptyCredentials, Credentials))
import Helpers.Database.Database (getVisits, uuidExists, getGuestbook, runDb)
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
import Data.Password.Bcrypt (PasswordCheck(PasswordCheckSuccess, PasswordCheckFail), mkPassword, checkPassword, PasswordHash (PasswordHash))
import Data.Text (pack, unpack)
import Crypto.Random (getRandomBytes)
import Data.Text.Array (Array(ByteArray))
import Text.StringRandom (stringRandomIO)
import Helpers.Logger (info)
import Helpers.Database.Schema (GuestbookEntry(guestbookEntryGuestbookTimestamp, guestbookEntryGuestbookName, guestbookEntryGuestbookParentId, guestbookEntryGuestbookContent, GuestbookEntry, guestbookEntryGuestbookId), Snake (Snake), User (User, userUserPassword), Token (Token, tokenTokenToken), Visit (Visit))
import Database.Persist (selectList, Entity (Entity), insertEntity)



handleGuestbookEntry :: T.GuestbookEntry -> IO (Status, String)
handleGuestbookEntry (T.GuestbookEntry "" _ _) = return (status400, "Error, name cannot be empty")
handleGuestbookEntry (T.GuestbookEntry _ "" _) = return (status400, "Error, content cannot be empty")
handleGuestbookEntry (T.GuestbookEntry name content parentId) = do
    time <- fmap round getPOSIXTime :: IO Int
    runDb $ insertEntity $ GuestbookEntry 0 time name content parentId
    return (status200, "Success")

handleLeaderboardEntry :: T.LeaderboardEntry -> IO (Status, String)
handleLeaderboardEntry (T.LeaderboardEntry name score speed fruits) = do
    time <- fmap round getPOSIXTime :: IO Int
    runDb $ insertEntity $ Snake 0 time name score speed fruits
    return (status200, "Success")
handleLeaderboardEntry T.EmptyLeaderboard = return (status400, "Error")

handleLogin :: T.Credentials -> IO (Status, String)
handleLogin (T.Credentials username password) = do
    let pass = mkPassword $ pack password
    rows <- map (\(Entity _ e) -> e) <$> (runDb $ selectList [] [] :: IO [Entity User])
    case rows of
        [user] -> case checkPassword pass (PasswordHash $ pack (userUserPassword user)) of
            PasswordCheckSuccess -> do
                rows <- map (\(Entity _ e) -> e) <$> (runDb $ selectList [] [] :: IO [Entity Token])
                if null rows then do
                    token <- stringRandomIO "[0-9a-zA-Z]{4}-[0-9a-ZA-Z]{10}-[0-9a-zA-Z]{15}"
                    runDb $ insertEntity $ Token 0 (unpack token) username
                    return (status200, unpack token)
                else do
                    let row = head rows
                    return (status200, tokenTokenToken row)
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
        runDb $ insertEntity $ Visit 0 time $ toString uuid
        info "Inserted into db"
        return (status200, toString uuid)
    else
        return (status200, unpackBS body)
api ["visits", "get"] request = do
    visits <- show . length <$> getVisits
    return (status200, visits)
api ["guestbook", "add"] request = do
    body <- getRequestBodyChunk request
    let entry = getDefault T.EmptyGuestbook (decode (fromStrict body) :: Maybe T.GuestbookEntry)
    handleGuestbookEntry entry
api ["guestbook", "get"] request = do
    body <- getRequestBodyChunk request
    entries <- getGuestbook
    return (status200, unpackBS $ toStrict $ encode $ show entries)
api ["snake", "add"] request = do
    body <- getRequestBodyChunk request
    let entry = getDefault T.EmptyLeaderboard (decode (fromStrict body) :: Maybe T.LeaderboardEntry)
    handleLeaderboardEntry entry
api ["admin", "login"] request = do
    body <- getRequestBodyChunk request
    let credentials = getDefault T.EmptyCredentials (decode (fromStrict body) :: Maybe T.Credentials)
    handleLogin credentials
api ["hello"] _ = do
    return (status200, "Hello World!")
api xs request = do
    return (status404, "{\"error\":\"Endpoint does not exist\"}")
