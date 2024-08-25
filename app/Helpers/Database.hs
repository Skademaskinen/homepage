{-# LANGUAGE OverloadedStrings #-}

module Helpers.Database where

import Database.SQLite.Simple

db_file :: String
db_file = "db.db3"

insert :: ToRow a => Query -> a -> IO ()
insert query args = do
    conn <- open db_file
    execute conn query args
    close conn

getVisits :: IO [(Int, Int, String)]
getVisits = do
    conn <- open db_file
    visits <- query conn "SELECT * FROM visits" () :: IO [(Int, Int, String)]
    close conn
    return visits

uuidExists :: String -> IO Bool
uuidExists uuid = do
    conn <- open db_file
    visits <- query conn "SELECT uuid FROM visits WHERE uuid = ?" (Only (uuid :: String)) :: IO [Only String]
    close conn
    return (not ((length visits) > 0))


init_db :: IO ()
init_db = do
    conn <- open db_file
    execute conn "CREATE TABLE IF NOT EXISTS visits (id INTEGER PRIMARY KEY, timestamp INTEGER NOT NULL, uuid VARCHAR NOT NULL)" ()
    close conn
    putStrLn "Initialized db..."

test_db :: IO ()
test_db = do
    conn <- open db_file
    execute conn "INSERT INTO visits (timestamp, uuid) VALUES (?, ?)" (0 :: Int, "1234" :: String)
    result <- query conn "SELECT * FROM visits" () :: IO [(Int, Int, String)]
    mapM_ print result
    close conn
