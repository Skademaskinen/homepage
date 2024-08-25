module Api.Api where

import Helpers.Database
import Helpers.Utils

import IHP.HSX.QQ (hsx)
import Text.Blaze.Html (Html)

import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.UUID.V4 (nextRandom)
import Data.UUID (toString)

import Network.Wai (getRequestBodyChunk, Request)


api :: [String] -> Request -> IO String
api ["visits", "new"] request = do
    body <- (getRequestBodyChunk request)
    result <- uuidExists (unpackBS body)
    if result then do
        time <- fmap round getPOSIXTime :: IO Int
        uuid <- nextRandom
        insert "INSERT INTO visits (timestamp, uuid) values (?, ?)" (time :: Int, toString uuid :: String)
        putStrLn "Inserted into db"
        return (toString uuid)
    else
        return (unpackBS body)
api ["visits", "get"] request = do
    result <- getVisits
    return (show (Prelude.length result))
api xs request = do
    return ""
