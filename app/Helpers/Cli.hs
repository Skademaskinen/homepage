module Helpers.Cli where
import System.Exit (exitSuccess)

import Helpers.Logger (up, right, clearLine, clearEnd)
import Data.List (intercalate)
import Data.Text (pack, unpack)
import System.IO (hFlush, stdout)
import Data.Password.Bcrypt (mkPassword, hashPassword, PasswordHash (PasswordHash))
import Helpers.Database.Database (runDb)
import Database.Persist (insertEntity, PersistQueryWrite (deleteWhere))
import Helpers.Database.Schema (User(User), EntityField (UserUserName))
import Database.Persist.MySQL ((==.))

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
            "exit:" ++ right 10 ++ "Exits the program",
            "help:" ++ right 10 ++ "Shows help about the CLI",
            "drop:" ++ right 10 ++ "Deletes a table and reruns init schema :: REMOVED",
            "adduser:" ++ right 7 ++ "Adds a user to the users table",
            "removeuser:" ++ right 4 ++ "removes a user from the users table"
            ]
doCommand ("exit":_) = do 
    putStrLn "Exiting"
    exitSuccess
doCommand ["adduser", username, password] = do
    let pass = mkPassword $ pack password
    (PasswordHash hash) <- hashPassword pass
    runDb $ insertEntity $ User 0 username $ unpack hash
    putStrLn "Successfully added user"
    resetCursor 2
    cli
doCommand ["removeuser", username] = do
    runDb $ deleteWhere [UserUserName ==. username]
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