{-# LANGUAGE OverloadedStrings #-}

module Api.Api where


import Helpers.Tables
import Helpers.Database (getVisits, uuidExists, insert, getGuestbook)
import Helpers.Utils (unpackBS, getDefault)

import IHP.HSX.QQ (hsx)
import Text.Blaze.Html (Html)

import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.UUID.V4 (nextRandom)
import Data.UUID (toString)

import Network.Wai (getRequestBodyChunk, Request)
import Network.HTTP.Types.Status (Status, status404, status200, status400)

import Data.Aeson
import Data.ByteString.Lazy (fromStrict, toStrict)
import Control.Applicative


handleGuestbookEntry :: GuestbookEntry -> IO (Status, String)
handleGuestbookEntry (GuestbookEntry name content parentId) = do
    time <- fmap round getPOSIXTime :: IO Int
    insert "INSERT INTO guestbook (name, timestamp, content, parentId) values (?, ?, ?, ?)" (name :: String, time :: Int, content :: String, parentId :: Int)
    return (status200, "Success")
handleGuestbookEntry EmptyGuestbook = do
    return (status400, "Error")

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
    return (status200, unpackBS $ toStrict $ encode entries)
api xs request = do
    return (status404, "{\"error\":\"Endpoint does not exist\"}")
