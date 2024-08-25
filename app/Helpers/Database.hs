{-# LANGUAGE OverloadedStrings #-}

module Helpers.Database where

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

db_file :: String
db_file = "db.db3"

data VisitsField = VisitsField Int Int String deriving Show

instance FromRow VisitsField where
    fromRow = VisitsField <$> field <*> field <*> field

insert :: ToRow a => Query -> a -> IO ()
insert query args = do
    conn <- open db_file
    execute conn query args
    close conn

get :: Query -> IO [(Int, Int, String)]
get query = do
    conn <- open db_file
    visits <- query_ conn query :: IO [VisitsField]
    let result = map (\(VisitsField id timestamp uuid) -> (id, timestamp, uuid)) visits
    return result

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
    result <- query_ conn "SELECT * FROM visits" :: IO [VisitsField]
    mapM_ print result
    close conn