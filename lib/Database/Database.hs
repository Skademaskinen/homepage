{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.Database where

import Settings (getDatabaseName, getDatabaseUser, getLocal)

import Control.Monad.Logger (NoLoggingT (runNoLoggingT))
import Data.List (inits, intercalate)
import Data.Text (Text, pack, unpack)
import Database.Persist.MySQL (ConnectInfo (ConnectInfo, connectDatabase, connectUser), Entity (Entity), EntityNameDB (unEntityNameDB), FieldDef (FieldDef), FieldNameHS (unFieldNameHS), Filter (Filter), FilterValue (FilterValue), PersistFilter (BackendSpecificFilter), PersistStoreWrite (insert_), SqlPersistT, defaultConnectInfo, fieldDBName, getEntityDBName, getEntityFields, runMigration, runSqlConn, selectList, withMySQLConn, SelectOpt, insertEntity)
import qualified Database.Persist.Sqlite as SQLite
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import Database.Persist.Types (EntityDef, FieldDef (fieldSqlType), fieldHaskell)
import Database.Persist ((==.), (=.))
import Database.Schema (EntityField (TokenToken, VisitUuid), GuestbookEntry (GuestbookEntry, guestbookEntryIndex), Snake (Snake, snakeIndex), Token (tokenName, tokenToken, Token, tokenIndex), User (userName, User, userIndex), Visit (Visit, visitIndex), defs, migrateAll)
import Logger (info)
import Tree (Tree (Tree))
import Text.Blaze.Html (Html)
import IHP.HSX.QQ (hsx)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.UUID.V4 (nextRandom)
import Data.UUID (toString)
import Database.Persist.Sqlite (withSqliteConn)

-- Database boilerplate

connectInfo :: IO ConnectInfo
connectInfo = do
    db <- getDatabaseName
    user <- getDatabaseUser
    return defaultConnectInfo { connectDatabase = db, connectUser = user }

runDb :: SqlPersistT (NoLoggingT IO) a -> IO a
runDb cmd = do
    info <- connectInfo
    local <- getLocal
    if local then
        runNoLoggingT . withSqliteConn "test.db3" . runSqlConn $ cmd
    else
        runNoLoggingT . withMySQLConn info . runSqlConn $ cmd


doMigration :: IO ()
doMigration = runDb $ runMigration migrateAll

-- getters

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
validateToken token = any (\x -> tokenToken x == token) <$> (getData [] [] :: IO [Token])

newVisit :: IO String
newVisit = do
    time <- fmap round getPOSIXTime :: IO Int
    uuid <- nextRandom
    runDb $ insertEntity $ Visit 0 time $ toString uuid
    return $ toString uuid

class AdminTable a where
    toList :: a -> [String]
    button :: a -> Html
    getData :: [Filter a] -> [SelectOpt a] -> IO [a] 
instance AdminTable Visit where
    toList (Visit rid timestamp uuid) = [show rid, show timestamp, uuid]
    button a = [hsx|<button id={"visits::"++(show $ visitIndex a)} onclick="delete_row(this.id)">Delete</button>|]
    getData f o = map (\(Entity _ e) -> e) <$> runDb (selectList f o) :: IO [Visit]  
instance AdminTable GuestbookEntry where
    toList (GuestbookEntry rid timestamp name content parentId) = [show rid, show timestamp, name, content, show parentId]
    button a = [hsx|<button id={"guestbook::"++(show $ guestbookEntryIndex a)} onclick="delete_row(this.id)">Delete</button>|]
    getData f o = map (\(Entity _ e) -> e) <$> runDb (selectList f o) :: IO [GuestbookEntry] 
instance AdminTable Snake where
    toList (Snake rid timestamp name score speed fruits) = [show rid, show timestamp, name, show score, show speed, show fruits]
    button a = [hsx|<button id={"snake::"++(show $ snakeIndex a)} onclick="delete_row(this.id)">Delete</button>|]
    getData f o = map (\(Entity _ e) -> e) <$> runDb (selectList f o) :: IO [Snake] 
instance AdminTable User where
    toList (User rid name password) = [show rid, name, password]
    button a = [hsx|<button id={"users::"++(show $ userIndex a)} onclick="delete_row(this.id)">Delete</button>|]
    getData f o = map (\(Entity _ e) -> e) <$> runDb (selectList f o) :: IO [User] 
instance AdminTable Token where
    toList (Token rid token name) = [show rid, token, name]
    button a = [hsx|<button id={"valid_tokens::"++(show $ tokenIndex a)} onclick="delete_row(this.id)">Delete</button>|]
    getData f o = map (\(Entity _ e) -> e) <$> runDb (selectList f o) :: IO [Token] 
