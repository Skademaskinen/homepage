{-# LANGUAGE OverloadedStrings #-}

module Settings where

import Data.List (find)
import Data.List.Split (splitOn)
import Data.Text (isInfixOf, pack, unpack)
import System.Environment (getArgs, lookupEnv)

argOrEnvOrDefault :: String -> String -> String -> IO String
argOrEnvOrDefault arg env def = do
    args <- getArgs
    case find (isInfixOf (pack arg)) [pack arg | arg <- args] of
        (Just x) -> return $ splitOn "=" (unpack x) !! 1
        _ -> do
            var <- lookupEnv env
            return $ case var of
                (Just a) -> a
                _ -> def

argOrEnvOrBool :: String -> String -> IO Bool
argOrEnvOrBool arg env = do
    args <- getArgs
    case find (== arg) args of
        (Just _) -> return True
        _ -> do
            var <- lookupEnv env
            return $ case var of
                (Just "1") -> True
                _ -> False

getDatabaseName :: IO String
getDatabaseName = argOrEnvOrDefault "--db" "HOMEPAGE_DB" "homepage"

getDatabaseUser :: IO String
getDatabaseUser = argOrEnvOrDefault "--dbuser" "HOMEPAGE_DB_USER" "homepage"

getPort :: IO Int
getPort = do
    var <- argOrEnvOrDefault "--port" "HOMEPAGE_PORT" "8000"
    return $ read var

data LogLevel = Error | Warning | Info

getLogLevel :: IO LogLevel
getLogLevel = do
    var <- argOrEnvOrDefault "--loglevel" "HOMEPAGE_LOGLEVEL" "error"
    return $ case var of
        "info" -> Info
        "warning" -> Warning
        "error" -> Error
        _ -> Error

getCliState :: IO Bool
getCliState = argOrEnvOrBool "--cli" "HOMEPAGE_CLI"

getColor :: IO Bool
getColor = argOrEnvOrBool "--color" "HOMEPAGE_COLOR"

getMigrate :: IO Bool
getMigrate = argOrEnvOrBool "--migrate" "HOMEPAGE_MIGRATE"

getEditorRoot :: IO String
getEditorRoot = argOrEnvOrDefault "--editor_root" "HOMEPAGE_EDITOR_ROOT" "./editor_root"

getLocal :: IO Bool
getLocal = argOrEnvOrBool "--local" "HOMEPAGE_LOCAL"
