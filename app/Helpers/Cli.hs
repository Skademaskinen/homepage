module Helpers.Cli where
import System.Exit (exitSuccess)

import Helpers.Logger (up, right, clearLine)
import Data.List (intercalate)
import Database.SQLite.Simple (Only (Only), execute)
import Helpers.Database (getConn)
import Database.SQLite.Simple.Types (Query(Query))
import Data.Text (pack)

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
    cli
doCommand x = do
    putStrLn $ "Error, no such command: ["++ unwords x ++"]"
    resetCursor 2
    cli

cli :: IO ()
cli = do
    putStr "> "
    line <- getLine
    if line /= "exit" then
        doCommand $ words line
    else do
        putStrLn "Exiting... press ^C now!"
        exitSuccess