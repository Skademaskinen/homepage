module Api.Api where

import Helpers.Database (getVisits, uuidExists, insert)
import Helpers.Utils (unpackBS)

import IHP.HSX.QQ (hsx)
import Text.Blaze.Html (Html)

import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.UUID.V4 (nextRandom)
import Data.UUID (toString)

import Network.Wai (getRequestBodyChunk, Request)
import Network.HTTP.Types.Status (Status, status404, status200)


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
api xs request = do
    return (status404, "{\"error\":\"Endpoint does not exist\"}")
