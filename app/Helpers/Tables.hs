module Helpers.Tables where

import Data.Aeson
import Data.ByteString.Lazy (fromStrict)
import Control.Applicative

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

data GuestbookEntry = GuestbookEntry {
    name :: String,
    content :: String,
    parent :: Int
} | EmptyGuestbook
    deriving Show

instance FromJSON GuestbookEntry where
    parseJSON (Object v) = GuestbookEntry <$>
                           v .: "name" <*>
                           v .: "content" <*>
                           v .: "parentId"
    parseJSON _          = empty

instance ToJSON GuestbookEntry where
    toJSON (GuestbookEntry name content parent) = object ["name" .= name, "content" .= content, "parent" .= parent]

instance FromRow GuestbookEntry where
    fromRow = GuestbookEntry <$> field <*> field <*> field

data LeaderboardEntry = LeaderboardEntry {
    lname :: String,
    score :: Int,
    speed :: Int,
    fruits :: Int
} | EmptyLeaderboard
    deriving Show

instance FromJSON LeaderboardEntry where
    parseJSON (Object v) = LeaderboardEntry <$>
                           v .: "name" <*>
                           v .: "score" <*>
                           v .: "speed" <*>
                           v .: "fruits"
    parseJSON _          = empty 