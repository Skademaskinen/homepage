{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.Database where

import Settings (getDatabaseName, getDatabaseUser, getDialect, getDatabasePassword, getDatabasePort, getDatabaseHost)

import Control.Monad.Logger (NoLoggingT (runNoLoggingT))
import Data.List (inits, intercalate)
import Data.Text (Text, pack, unpack)
import Database.Persist.MySQL (ConnectInfo (ConnectInfo, connectDatabase, connectUser), Entity (Entity), EntityNameDB (unEntityNameDB), FieldDef (FieldDef), FieldNameHS (unFieldNameHS), Filter (Filter), FilterValue (FilterValue), PersistFilter (BackendSpecificFilter), PersistStoreWrite (insert_), SqlPersistT, defaultConnectInfo, fieldDBName, getEntityDBName, getEntityFields, runMigration, runSqlConn, selectList, withMySQLConn, SelectOpt, insertEntity)
import qualified Database.Persist.Sqlite as SQLite
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import Database.Persist.Types (EntityDef, FieldDef (fieldSqlType), fieldHaskell)
import Database.Persist ((==.), (=.))
import Database.Persist.Sql (BackendKey (SqlBackendKey), getBy)
import Database.Schema (EntityField (TokenToken, VisitUuid, VisitTimestamp, GuestbookEntryTimestamp, SnakeTimestamp, EventDate, EventCancelled), GuestbookEntry (GuestbookEntry, guestbookEntryTimestamp), Snake (Snake, snakeTimestamp), Token (tokenName, tokenToken, Token), User (userName, User), Visit (Visit, visitTimestamp, visitUuid), defs, migrateAll, Key (VisitKey, GuestbookEntryKey, SnakeKey, UserKey, TokenKey, MemberKey, EventKey), Unique (ValidToken), Member (Member), Event (Event, eventDate))
import Logger (info)
import Tree (Tree (Tree))
import Text.Blaze.Html (Html)
import IHP.HSX.QQ (hsx)
import Data.ByteString.UTF8 (fromString)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.UUID.V4 (nextRandom)
import Data.UUID (toString)
import Database.Persist.Sqlite (withSqliteConn)
import Database.Persist.Postgresql (withPostgresqlConn)

-- Database boilerplate

connectInfo :: IO ConnectInfo
connectInfo = do
    db <- getDatabaseName
    user <- getDatabaseUser
    return defaultConnectInfo { connectDatabase = db, connectUser = user }

connectString :: IO String
connectString = do
    db <- getDatabaseName
    user <- getDatabaseUser
    host <- getDatabaseHost
    port <- getDatabasePort
    password <- getDatabasePassword
    return $ "host="++host++" port="++port++" user="++user++" dbname="++db++" password="++password

runDb :: SqlPersistT (NoLoggingT IO) a -> IO a
runDb cmd = do
    info <- connectInfo
    string <- fromString <$> connectString
    dialect <- getDialect
    case dialect of
        "sqlite" -> runNoLoggingT . withSqliteConn "test.db3" . runSqlConn $ cmd
        "mysql" -> runNoLoggingT . withMySQLConn info . runSqlConn $ cmd
        "postgresql" -> runNoLoggingT . withPostgresqlConn string . runSqlConn $ cmd

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
--validateToken token = any (\(Entity id x) -> tokenToken x == token) <$> getRows [] []
validateToken token = runDb $ do 
    validToken <- getBy (ValidToken token) 
    case validToken of
        Nothing -> return False
        _ -> return True

newVisit :: IO String
newVisit = do
    time <- fmap round getPOSIXTime :: IO Int
    uuid <- nextRandom
    runDb $ insertEntity $ Visit time $ toString uuid
    return $ toString uuid

class AdminTable a where
    toList :: Entity a -> [String]
    makeButton :: Entity a -> Html
    getId :: Key a -> Int
    getRows :: [Filter a] -> [SelectOpt a] -> IO [Entity a]
    getList :: [Filter a] -> [SelectOpt a] -> IO [a]
    getList f o = map (\(Entity _ e) -> e) <$> getRows f o
    getLast :: IO a
    getLast = do
        last <$> getAll
    getAll :: IO [a]
    getAll = getList [] []
instance AdminTable Visit where
    toList (Entity id (Visit timestamp uuid)) = [show $ getId id, show timestamp, uuid]
    makeButton (Entity id a) = [hsx|<button id={"visits::"++(show $ getId id)} onclick="delete_row(this.id)">Delete</button>|]
    getRows f o = runDb (selectList f o)
    getId (VisitKey (SqlBackendKey id)) = fromIntegral id
    getLast = do
        rows <- getAll
        let timestamps = map visitTimestamp rows
        head <$> getList [VisitTimestamp ==. maximum timestamps] []

instance AdminTable GuestbookEntry where
    toList (Entity id (GuestbookEntry timestamp name content parentId)) = [show $ getId id, show timestamp, name, content, show parentId]
    makeButton (Entity id a) = [hsx|<button id={"guestbook::"++(show $ getId id)} onclick="delete_row(this.id)">Delete</button>|]
    getRows f o = runDb (selectList f o)
    getId (GuestbookEntryKey (SqlBackendKey id)) = fromIntegral id
    getLast = do
        rows <- getAll
        let timestamps = map guestbookEntryTimestamp rows
        head <$> getList [GuestbookEntryTimestamp ==. maximum timestamps] []
instance AdminTable Snake where
    toList (Entity id (Snake timestamp name score speed fruits)) = [show $ getId id, show timestamp, name, show score, show speed, show fruits]
    makeButton (Entity id a) = [hsx|<button id={"snake::"++(show $ getId id)} onclick="delete_row(this.id)">Delete</button>|]
    getRows f o = runDb (selectList f o)
    getId (SnakeKey (SqlBackendKey id)) = fromIntegral id
    getLast = do
        rows <- getAll
        let timestamps = map snakeTimestamp rows
        head <$> getList [SnakeTimestamp ==. maximum timestamps] []
instance AdminTable User where
    toList (Entity id (User name password)) = [show $ getId id, name, password]
    makeButton (Entity id a) = [hsx|<button id={"users::"++(show $ getId id)} onclick="delete_row(this.id)">Delete</button>|]
    getRows f o = runDb (selectList f o)
    getId (UserKey (SqlBackendKey id)) = fromIntegral id
instance AdminTable Token where
    toList (Entity id (Token token name)) = [show $ getId id, token, name]
    makeButton (Entity id a) = [hsx|<button id={"valid_tokens::"++(show $ getId id)} onclick="delete_row(this.id)">Delete</button>|]
    getRows f o = runDb (selectList f o)
    getId (TokenKey (SqlBackendKey id)) = fromIntegral id
instance AdminTable Member where
    toList (Entity id (Member name)) = [show $ getId id, name]
    makeButton (Entity id a) = [hsx|<button id={"member::"++(show $ getId id)} onclick="delete_row(this.id)">Delete</button>|]
    getRows f o = runDb (selectList f o)
    getId (MemberKey (SqlBackendKey id)) = fromIntegral id
instance AdminTable Event where
    toList (Entity id (Event date responsible cancelled)) = [show $ getId id, show date, show responsible, show cancelled]
    makeButton (Entity id a) = [hsx|<button id={"event::"++(show $ getId id)} onclick="delete_row(this.id)">Delete</button>|]
    getRows f o = runDb (selectList f o)
    getId (EventKey (SqlBackendKey id)) = fromIntegral id
    getLast = do
        rows <- getAll
        let dates = map eventDate rows
        head <$> getList [EventDate ==. maximum dates, EventCancelled ==. False] []
