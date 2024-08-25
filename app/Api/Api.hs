module Api.Api where

import Helpers.Database

import IHP.HSX.QQ
import Text.Blaze.Html

api :: [String] -> IO String
api ["visits", "new"] = do
    insert "INSERT INTO visits (timestamp, uuid) values (?, ?)" (0 :: Int, "inserted from web!" :: String)
    putStrLn "Inserted into db"
    return ""
api ["visits", "get"] = do
    result <- get "SELECT * FROM visits"
    return (show (length result))
api xs = do
    return ""