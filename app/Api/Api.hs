{-# LANGUAGE OverloadedStrings #-}

module Api.Api where


import qualified Tables as T (GuestbookEntry (GuestbookEntry, EmptyGuestbook), LeaderboardEntry (EmptyLeaderboard, LeaderboardEntry), Credentials (EmptyCredentials, Credentials))
import Database.Database (getVisits, uuidExists, getGuestbook, runDb)
import Utils (unpackBS, getDefault)
import Pages.Projects.Brainfuck (code)

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
import Logger (info)
import Database.Schema (GuestbookEntry(guestbookEntryGuestbookTimestamp, guestbookEntryGuestbookName, guestbookEntryGuestbookParentId, guestbookEntryGuestbookContent, GuestbookEntry, guestbookEntryGuestbookId), Snake (Snake), User (User, userUserPassword, userUserName), Token (Token, tokenTokenToken), Visit (Visit), EntityField (UserUserName, TokenTokenName))
import Database.Persist (selectList, Entity (Entity), insertEntity, Filter (Filter), FilterValue (FilterValue), PersistFilter (BackendSpecificFilter))

import Network.HTTP.Types (HeaderName)
import Data.ByteString.UTF8 (ByteString)
import Data.Aeson.QQ.Simple (aesonQQ)

type Header = (HeaderName, ByteString)
type APIResponse = IO (Status, String, [Header])

j2s :: Value -> String
j2s = unpackBS . toStrict .encode

defaultHeaders :: [Header]
defaultHeaders = [("Content-Type", "text/plain")]

messageResponse :: String -> String
messageResponse value = j2s [aesonQQ|{
    "message":"#{value}"
}|]


handleGuestbookEntry :: T.GuestbookEntry -> APIResponse
handleGuestbookEntry (T.GuestbookEntry "" _ _) = return (status400, "Error, name cannot be empty", defaultHeaders)
handleGuestbookEntry (T.GuestbookEntry _ "" _) = return (status400, "Error, content cannot be empty", defaultHeaders)
handleGuestbookEntry (T.GuestbookEntry name content parentId) = do
    time <- fmap round getPOSIXTime :: IO Int
    runDb $ insertEntity $ GuestbookEntry 0 time name content parentId
    return (status200, messageResponse "Success" , defaultHeaders)

handleLeaderboardEntry :: T.LeaderboardEntry -> APIResponse
handleLeaderboardEntry (T.LeaderboardEntry name score speed fruits) = do
    time <- fmap round getPOSIXTime :: IO Int
    runDb $ insertEntity $ Snake 0 time name score speed fruits
    return (status200, messageResponse "Success", defaultHeaders)
handleLeaderboardEntry T.EmptyLeaderboard = return (status400, messageResponse "Error", defaultHeaders)

handleLogin :: T.Credentials -> APIResponse
handleLogin (T.Credentials username password) = do
    let pass = mkPassword $ pack password
    rows <- map (\(Entity _ e) -> e) <$> (runDb $ selectList [Filter UserUserName (FilterValue username) (BackendSpecificFilter "LIKE")] [] :: IO [Entity User])
    case rows of
        [user] -> case checkPassword pass (PasswordHash $ pack (userUserPassword user)) of
            PasswordCheckSuccess -> do
                rows <- map (\(Entity _ e) -> e) <$> (runDb $ selectList [Filter TokenTokenName (FilterValue username) (BackendSpecificFilter "LIKE")] [] :: IO [Entity Token])
                if null rows then do
                    token <- stringRandomIO "[0-9a-zA-Z]{4}-[0-9a-ZA-Z]{10}-[0-9a-zA-Z]{15}"
                    runDb $ insertEntity $ Token 0 (unpack token) username
                    return (status200, unpack token, defaultHeaders)
                else do
                    let row = head rows
                    return (status200, tokenTokenToken row, defaultHeaders)
                    where
            PasswordCheckFail -> return (status400, messageResponse "Wrong username or password", defaultHeaders)
        _ -> return (status400, messageResponse "Error, no user exists" , defaultHeaders)
handleLogin _ = return (status400, messageResponse "Invalid request", defaultHeaders)

api :: [String] -> Request -> APIResponse
api ["visits", "new"] request = do
    body <- getRequestBodyChunk request
    result <- uuidExists (unpackBS body)
    if result then do
        time <- fmap round getPOSIXTime :: IO Int
        uuid <- nextRandom
        runDb $ insertEntity $ Visit 0 time $ toString uuid
        info "Inserted into db"
        return (status200, toString uuid, defaultHeaders)
    else
        return (status200, unpackBS body, defaultHeaders)
api ["visits", "get"] request = do
    visits <- show . length <$> getVisits
    return (status200, visits, [])
api ["guestbook", "add"] request = do
    body <- getRequestBodyChunk request
    let entry = getDefault T.EmptyGuestbook (decode (fromStrict body) :: Maybe T.GuestbookEntry)
    handleGuestbookEntry entry
api ["guestbook", "get"] request = do
    body <- getRequestBodyChunk request
    entries <- getGuestbook
    return (status200, unpackBS $ toStrict $ encode $ show entries, defaultHeaders)
api ["snake", "add"] request = do
    body <- getRequestBodyChunk request
    let entry = getDefault T.EmptyLeaderboard (decode (fromStrict body) :: Maybe T.LeaderboardEntry)
    handleLeaderboardEntry entry
api ["admin", "login"] request = do
    body <- getRequestBodyChunk request
    let credentials = getDefault T.EmptyCredentials (decode (fromStrict body) :: Maybe T.Credentials)
    handleLogin credentials
api ["hello"] _ = do
    return (status200, j2s [aesonQQ|{
        "message":"Hello World!"
    }|], defaultHeaders)
api ["brainfuck"] request = do
    input <- getRequestBodyChunk request
    let result = code $ unpackBS input
    return (status200, result, [("Content-Disposition", "attachment; filename=\"brainfuck.c\"")])

api xs request = do
    return (status404, messageResponse "Endpoint does not exist", defaultHeaders)
