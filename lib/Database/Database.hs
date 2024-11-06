{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.Database where

import Settings (getDatabaseName, getDatabaseUser)

import Control.Monad.Logger (NoLoggingT (runNoLoggingT))
import Data.List (inits, intercalate)
import Data.Text (Text, pack, unpack)
import Database.Persist.MySQL (ConnectInfo (ConnectInfo, connectDatabase, connectUser), Entity (Entity), EntityNameDB (unEntityNameDB), FieldDef (FieldDef), FieldNameHS (unFieldNameHS), Filter (Filter), FilterValue (FilterValue), PersistFilter (BackendSpecificFilter), PersistStoreWrite (insert_), SqlPersistT, defaultConnectInfo, fieldDBName, getEntityDBName, getEntityFields, runMigration, runSqlConn, selectList, withMySQLConn)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import Database.Persist.Types (EntityDef, FieldDef (fieldSqlType), fieldHaskell)
import Database.Persist ((==.))
import Database.Schema (EntityField (TokenToken, VisitUuid), GuestbookEntry (GuestbookEntry, guestbookEntryRid), Snake (Snake, snakeRid), Token (tokenName, tokenToken, Token, tokenRid), User (userName, User, userRid), Visit (Visit, visitRid), defs, migrateAll)
import Logger (info)
import Tree (Tree (Tree))
import Text.Blaze.Html (Html)
import IHP.HSX.QQ (hsx)

-- Database boilerplate

connectInfo :: IO ConnectInfo
connectInfo = do
    db <- getDatabaseName
    user <- getDatabaseUser
    return defaultConnectInfo { connectDatabase = db, connectUser = user }

runDb :: SqlPersistT (NoLoggingT IO) a -> IO a
runDb cmd = do
    info <- connectInfo
    runNoLoggingT . withMySQLConn info . runSqlConn $ cmd

doMigration :: IO ()
doMigration = runDb $ runMigration migrateAll

-- utils
guestbookToTree :: [GuestbookEntry] -> Int -> [Tree GuestbookEntry]
guestbookToTree entries targetParent = [Tree (GuestbookEntry id timestamp name content parent) $ guestbookToTree entries id | (GuestbookEntry id timestamp name content parent) <- entries, parent == targetParent]

-- getters

getVisits :: IO [Visit]
getVisits = do
    visits <- runDb $ selectList [] []
    return $ map (\(Entity _ u) -> u) visits

getGuestbook :: IO [Tree GuestbookEntry]
getGuestbook = do
    entries <- runDb $ selectList [] [] :: IO [Entity GuestbookEntry]
    return $ guestbookToTree (map (\(Entity _ entry) -> entry) entries) (-1)

getGuestbookEntries :: IO [GuestbookEntry]
getGuestbookEntries = do
    entries <- runDb $ selectList [] []
    return $ map (\(Entity _ e) -> e) entries

getLeaderboard :: IO [Snake]
getLeaderboard = do
    entries <- runDb $ selectList [] []
    return $ map (\(Entity _ e) -> e) entries

getUsers :: IO [User]
getUsers = do
    entries <- runDb $ selectList [] []
    return $ map (\(Entity _ e) -> e) entries

getTokens :: IO [Token]
getTokens = do
    entries <- runDb $ selectList [] []
    return $ map (\(Entity _ e) -> e) entries

uuidExists :: String -> IO Bool
uuidExists uuid = do
    visits <- runDb $ selectList [VisitUuid ==. uuid] [] :: IO [Entity Visit]
    print visits
    return (null visits)

tokenToUsername :: String -> IO String
tokenToUsername token = do
    (Entity _ token : _) <- runDb $ selectList [TokenToken ==. token] []
    return $ tokenName token

-- schema
prettyPrintSchema :: String
prettyPrintSchema =
    intercalate "\n" $ map (\(def :: EntityDef) -> 
        dbName def ++ unwords (map (\(field :: FieldDef) ->
            "\n\t" ++ fieldName field ++ replicate (20 - length (fieldName field)) ' ' ++ fieldType field
        ) $ getEntityFields def)
    ) defs
    where
        dbName entity = unpack $ unEntityNameDB $ getEntityDBName entity
        fieldName field = unpack $ unFieldNameHS $ fieldHaskell field
        fieldType field = show $ fieldSqlType field

validateToken :: String -> IO Bool
validateToken token = any (\x -> tokenToken x == token) <$> getTokens

class AdminTable a where
    toList :: a -> [String]
    button :: a -> Html
    getData :: [Filter a] -> IO [a] 
instance AdminTable Visit where
    toList (Visit rid timestamp uuid) = [show rid, show timestamp, uuid]
    button a = [hsx|<button id={"visits::"++(show $ visitRid a)} onclick="delete_row(this.id)">Delete</button>|]
    getData f = map (\(Entity _ e) -> e) <$> runDb (selectList f []) :: IO [Visit]  
instance AdminTable GuestbookEntry where
    toList (GuestbookEntry rid timestamp name content parentId) = [show rid, show timestamp, name, content, show parentId]
    button a = [hsx|<button id={"guestbook::"++(show $ guestbookEntryRid a)} onclick="delete_row(this.id)">Delete</button>|]
    getData f = map (\(Entity _ e) -> e) <$> runDb (selectList f []) :: IO [GuestbookEntry] 
instance AdminTable Snake where
    toList (Snake rid timestamp name score speed fruits) = [show rid, show timestamp, name, show score, show speed, show fruits]
    button a = [hsx|<button id={"snake::"++(show $ snakeRid a)} onclick="delete_row(this.id)">Delete</button>|]
    getData f = map (\(Entity _ e) -> e) <$> runDb (selectList f []) :: IO [Snake] 
instance AdminTable User where
    toList (User rid name password) = [show rid, name, password]
    button a = [hsx||<button id={"users::"++(show $ userRid a)} onclick="delete_row(this.id)">Delete</button>|]
    getData f = map (\(Entity _ e) -> e) <$> runDb (selectList f []) :: IO [User] 
instance AdminTable Token where
    toList (Token rid token name) = [show rid, token, name]
    button a = [hsx|<button id={"valid_tokens::"++(show $ tokenRid a)} onclick="delete_row(this.id)">Delete</button>|]
    getData f = map (\(Entity _ e) -> e) <$> runDb (selectList f []) :: IO [Token] 
