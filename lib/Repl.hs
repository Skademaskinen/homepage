module Repl where

import System.Exit (exitSuccess)

import Data.List (intercalate)
import Data.Password.Bcrypt (PasswordHash (PasswordHash), hashPassword, mkPassword)
import Data.Text (pack, unpack)
import Database.Database (runDb)
import Database.Persist (PersistQueryWrite (deleteWhere), insertEntity)
import Database.Persist.MySQL ((==.))
import Database.Schema (EntityField (UserName), User (User))
import Logger (clearEnd, clearLine, right, up)
import System.IO (hFlush, stdout)

resetCursor :: Int -> IO ()
resetCursor n = do
    putStr $ mconcat [up n]

doCommand :: [String] -> IO ()
doCommand ("help" : _) = do
    putStrLn $ intercalate "\n" help
    resetCursor $ length help + 1
    repl
    where
        help = [
            "Homepage CLI command list:", 
            "",
            "exit:" ++ right 10 ++ "Exits the program", 
            "help:" ++ right 10 ++ "Shows help about the CLI", 
            "drop:" ++ right 10 ++ "Deletes a table and reruns init schema :: REMOVED",
            "adduser:" ++ right 7 ++ "Adds a user to the users table",
            "removeuser:" ++ right 4 ++ "removes a user from the users table"
            ]
doCommand ("exit" : _) = do
    putStrLn "Exiting"
    exitSuccess
doCommand ["adduser", username, password] = do
    let pass = mkPassword $ pack password
    (PasswordHash hash) <- hashPassword pass
    runDb $ insertEntity $ User 0 username $ unpack hash
    putStrLn "Successfully added user"
    resetCursor 2
    repl
doCommand ["removeuser", username] = do
    runDb $ deleteWhere [UserName ==. username]
    putStrLn "Successfully removed user"
    resetCursor 2
    repl
doCommand x = do
    putStrLn $ "Error, no such command: [" ++ unwords x ++ "]"
    resetCursor 2
    repl

repl :: IO ()
repl = do
    putStr $ "> " ++ clearLine
    hFlush stdout
    line <- getLine
    putStr clearEnd
    hFlush stdout
    doCommand $ words line
