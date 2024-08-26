module Helpers.Globals where

import System.Environment ( lookupEnv )


getDbPath :: IO String
getDbPath = do
    result <- lookupEnv "HOMEPAGE_DB"
    return $ case result of
        (Just a) -> a
        Nothing -> "./homepage.db3"

getPort :: IO Int
getPort = do
    result <- lookupEnv "HOMEPAGE_PORT"
    return $ case result of
        (Just a) -> read a :: Int
        Nothing -> 8000