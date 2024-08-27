{-# LANGUAGE OverloadedStrings #-}

module Helpers.Database where

import Database.SQLite.Simple (close, execute, open, query, Only(Only), ToRow, Query (Query), Connection)

import Helpers.Globals (getDbPath)

import Data.List (intercalate, inits)
import Data.Text (pack, Text)

getConn :: IO Connection
getConn = do
    path <- getDbPath
    open path

insert :: ToRow a => Query -> a -> IO ()
insert query args = do
    conn <- getConn
    execute conn query args
    close conn

getVisits :: IO [(Int, Int, String)]
getVisits = do
    conn <- getConn
    visits <- query conn "SELECT * FROM visits" () :: IO [(Int, Int, String)]
    close conn
    return visits

getGuestbook :: IO [(Int, Int, String, String, Int)]
getGuestbook = do
    conn <- getConn
    entries <- query conn "SELECT * FROM guestbook" () :: IO [(Int, Int, String, String, Int)]
    close conn
    return entries

getLeaderboard :: IO [(Int, Int, String, Int, Int, Int)]
getLeaderboard = do
    conn <- getConn
    entries <- query conn "SELECT * FROM snake" () :: IO [(Int, Int, String, Int, Int, Int)]
    close conn
    return entries

uuidExists :: String -> IO Bool
uuidExists uuid = do
    conn <- getConn
    visits <- query conn "SELECT uuid FROM visits WHERE uuid = ?" (Only (uuid :: String)) :: IO [Only String]
    close conn
    return (null visits)

data Column = Column String String deriving Show
data Table = Table String [Column] deriving Show

schema :: [Table]
schema = [
    Table "visits" [
        Column "id" "INTEGER PRIMARY KEY",
        Column "timestamp" "INTEGER NOT NULL",
        Column "uuid" "VARCHAR NOT NULL"
    ],
    Table "guestbook" [
        Column "id" "INTEGER PRIMARY KEY",
        Column "timestamp" "INTEGER NOT NULL",
        Column "name" "VARCHAR NOT NULL",
        Column "content" "VARCHAR NOT NULL",
        Column "parentId" "INTEGER NOT NULL"
    ],
    Table "snake" [
        Column "id" "INTEGER PRIMARY KEY",
        Column "timestamp" "INTEGER NOT NULL",
        Column "name" "VARCHAR NOT NULL",
        Column "score" "INTEGER NOT NULL",
        Column "speed" "INTEGER NOT NULL",
        Column "fruits" "INTEGER NOT NULL"
    ]]

prettyPrintSchema :: String
prettyPrintSchema = intercalate "\n" $ map (\(Table name columns) -> name ++ unwords (map (\(Column cname opts) -> "\n\t"++cname++" "++opts) columns)) schema

columnText :: [Column] -> String
columnText columns = intercalate ", " $ map (\(Column name opts) -> unwords [name, opts]) columns

initSchema :: [Table] -> IO ()
initSchema ((Table name columns):tables) = do
    conn <- getConn
    execute conn (Query (pack $ "CREATE TABLE IF NOT EXISTS "++name++"("++columnText columns++")")) ()
    close conn
    putStrLn $ "Initialized table " ++ name
    initSchema tables
initSchema [] = putStrLn "Finished initializing DB"

initDb :: IO ()
initDb = do
    putStrLn "Initializing DB"
    initSchema schema

test_db :: IO ()
test_db = do
    conn <- getConn
    execute conn "INSERT INTO visits (timestamp, uuid) VALUES (?, ?)" (0 :: Int, "1234" :: String)
    result <- query conn "SELECT * FROM visits" () :: IO [(Int, Int, String)]
    mapM_ print result
    close conn
