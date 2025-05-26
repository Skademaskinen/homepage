module Repl where

import System.Exit (exitSuccess)

import Data.List (intercalate)
import Data.Password.Bcrypt (PasswordHash (PasswordHash), hashPassword, mkPassword)
import Data.Text (pack, unpack)
import Database.Database (runDb, AdminTable (getRows, toList, getAll), doMigration)
import Database.Persist (PersistQueryWrite (deleteWhere), insertEntity, SelectOpt (LimitTo), Entity (Entity, entityKey), PersistStoreWrite (insert, delete, update), PersistQueryRead (selectFirst), PersistStoreRead (get))
import Database.Persist.MySQL ((==.), (=.), toSqlKey)
import Database.Schema (EntityField (UserName, MemberName, EventId, EventCancelled), User (User), Visit (Visit), GuestbookEntry (GuestbookEntry), Snake (Snake), Token (Token), Member (Member, memberName), Key (EventKey), Event (Event, eventCancelled))
import Logger (clearEnd, clearLine, right, up)
import System.IO (hFlush, stdout)
import Text.RawString.QQ (r)
import Utils (count)

resetCursor :: Int -> IO ()
resetCursor n = do
    putStr $ mconcat [up n]

doCommand :: [String] -> IO ()
doCommand ("help" : _) = do
    putStrLn help
    resetCursor $ count '\n' help +2
    repl
    where
        help :: String
        help = [r|
            Homepage CLI command list:

            exit:           Exits the program
            help:           Shows help about the CLI
            drop:           Deletes a table and reruns init schema :: REMOVED
            adduser:        Adds a user to the users table
            show:           Show a table
            migrate:        Migrate the database
            removeuser:     Removes a user from the users table
            addmember:      add a member to the folkevogn
            deletemember:   delete a member from the folkevogn
            members:        show members currently registered for the folkevogn
            deleteevent:    delete an event
            toggleevent:    toggle cancellation of event
            events:         show all events
        |]
doCommand ("exit" : _) = do
    putStrLn "Exiting"
    exitSuccess
doCommand ["adduser", username, password] = do
    let pass = mkPassword $ pack password
    (PasswordHash hash) <- hashPassword pass
    runDb $ insertEntity $ User username $ unpack hash
    putStrLn "Successfully added user"
    resetCursor 2
    repl
doCommand ["removeuser", username] = do
    runDb $ deleteWhere [UserName ==. username]
    putStrLn "Successfully removed user"
    resetCursor 2
    repl
doCommand ["show", table, lines] = do
    let lineCount = read lines :: Int
    rows <- case table of
        "visits" -> do
            fmap toList <$> (getRows [] [LimitTo lineCount] :: IO [Entity Visit])
        "guestbook" -> do
            fmap toList <$> (getRows [] [LimitTo lineCount] :: IO [Entity GuestbookEntry])
        "snake" -> do
            fmap toList <$> (getRows [] [LimitTo lineCount] :: IO [Entity Snake])
        "users" -> do
            fmap toList <$> (getRows [] [LimitTo lineCount] :: IO [Entity User])
        "tokens" -> do
            fmap toList <$> (getRows [] [LimitTo lineCount] :: IO [Entity Token])
        _ -> do
            return [["Error, no such table"]]

    putStrLn $ intercalate "\n" $ map unwords rows
    resetCursor $ length rows + 1
    repl
doCommand ["show", table] = doCommand ["show", table, "5"]
doCommand ["migrate"] = do
    doMigration
    resetCursor 1
    repl
doCommand ["addmember", name] = do
    runDb . insert $ Member name
    resetCursor 1
    repl
doCommand ["deletemember", name] = do
    id <- entityKey . head <$> (getRows [MemberName ==. name] [] :: IO [Entity Member])
    runDb . delete $ id
    resetCursor 1
    repl
doCommand ["members"] = do
    members <- getAll :: IO [Member]
    putStrLn $ intercalate "\n" $ map memberName members
    resetCursor $ length members + 1
    repl
doCommand ["deleteevent", id] = do
    runDb . delete $ (toSqlKey (read id) :: Key Event)
    resetCursor 1
    repl
doCommand ["events"] = do
    events <- map toList <$> (getRows [] [] :: IO [Entity Event])
    putStrLn $ intercalate "\n" $ map (intercalate "\t|\t\t") events
    resetCursor $ length events + 1
    repl
doCommand ["toggleevent", id] = do
    maybeEvent <- runDb $ get (toSqlKey (read id) :: Key Event)
    case maybeEvent of
        (Just event) -> do
            let status = eventCancelled event
            runDb $ update (toSqlKey (read id) :: Key Event) [EventCancelled =. not status]
            resetCursor 1
        Nothing -> do
            putStrLn "Error, no such id"
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
