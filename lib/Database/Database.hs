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
import Database.Schema (EntityField (TokenToken, VisitUuid), GuestbookEntry (GuestbookEntry), Snake, Token (tokenName), User (userName), Visit, defs, migrateAll)
import Logger (info)
import Tree (Tree (Tree))

-- Database boilerplate

connectInfo :: IO ConnectInfo
connectInfo = do
  db <- getDatabaseName
  user <- getDatabaseUser
  return
    defaultConnectInfo
      { connectDatabase = db
      , connectUser = user
      }

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
  visits <- runDb $ selectList [Filter VisitUuid (FilterValue uuid) (BackendSpecificFilter "LIKE")] [] :: IO [Entity Visit]
  print visits
  return (null visits)

tokenToUsername :: String -> IO String
tokenToUsername token = do
  (Entity _ token : _) <- runDb $ selectList [Filter TokenToken (FilterValue token) (BackendSpecificFilter "LIKE")] []
  return $ tokenName token

-- schema
prettyPrintSchema :: String
prettyPrintSchema =
  intercalate "\n" $
    map
      ( \(def :: EntityDef) ->
          dbName def
            ++ unwords
              ( map
                  ( \(field :: FieldDef) ->
                      "\n\t" ++ fieldName field ++ replicate (20 - length (fieldName field)) ' ' ++ fieldType field
                  )
                  $ getEntityFields def
              )
      )
      defs
 where
  dbName :: EntityDef -> String
  dbName entity = unpack $ unEntityNameDB $ getEntityDBName entity
  fieldName :: FieldDef -> String
  fieldName field = unpack $ unFieldNameHS $ fieldHaskell field
  fieldType :: FieldDef -> String
  fieldType field = show $ fieldSqlType field

validateToken :: String -> IO Bool
validateToken token = do
  tokens <- getTokens
  case tokens of
    [] -> return False
    _ -> return True
