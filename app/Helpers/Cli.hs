module Helpers.Cli where
import System.Exit (exitSuccess)

import Helpers.Logger (up, right, clearLine, clearEnd)
import Data.List (intercalate)
import Database.SQLite.Simple (Only (Only), execute)
import Helpers.Database (getConn, initDb, insert)
import Database.SQLite.Simple.Types (Query(Query))
import Data.Text (pack, unpack)
import System.IO (hFlush, stdout)
import Data.Password.Bcrypt (mkPassword, hashPassword, PasswordHash (PasswordHash))

resetCursor :: Int ->  IO ()
resetCursor n = do
    putStr $ mconcat [up n]

doCommand :: [String] -> IO ()
doCommand ("help":_) = do
    putStrLn $ intercalate "\n" help
    resetCursor $ length help+1
    cli
    where
        help = [
            "Homepage CLI command list:",
            "",
            "exit:"++ right 10 ++ "Exits the program",
            "help:"++ right 10 ++ "Shows help about the CLI",
            "drop:"++ right 10 ++ "Deletes a table and reruns init schema"
            ]
doCommand ["drop", table] = do
    conn <- getConn
    execute conn (Query $ pack ("DROP TABLE "++table)) ()
    putStrLn "Successfully dropped table"
    resetCursor 2
    initDb
    cli
doCommand ("exit":_) = do 
    putStrLn "Exiting"
    exitSuccess
doCommand ["adduser", username, password] = do
    let pass = mkPassword $ pack password
    (PasswordHash hash) <- hashPassword pass
    insert "INSERT INTO users(username, password) VALUES (?, ?)" (username :: String, unpack hash :: String)
    putStrLn "Successfully added user"
    resetCursor 2
    cli
doCommand ["removeuser", username] = do
    conn <- getConn
    execute conn "DELETE FROM users WHERE username = ?" (Only username)
    putStrLn "Successfully removed user"
    resetCursor 2
    cli
doCommand x = do
    putStrLn $ "Error, no such command: ["++ unwords x ++"]"
    resetCursor 2
    cli

cli :: IO ()
cli = do
    putStr $ "> " ++ clearLine
    hFlush stdout
    line <- getLine
    putStr clearEnd
    hFlush stdout
    doCommand $ words line