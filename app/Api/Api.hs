module Api.Api where

import Helpers.Database

import IHP.HSX.QQ
import Text.Blaze.Html

import Data.Time.Clock.POSIX
import Data.UUID.V4
import Data.UUID

import Data.Text as T
import Data.Text.Encoding as T
import Data.ByteString

import Network.Wai

unpackBS :: ByteString -> String
unpackBS = (T.unpack . T.decodeUtf8)

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
