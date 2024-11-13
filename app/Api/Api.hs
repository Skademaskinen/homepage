{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Api.Api where

import Database.Database (runDb, AdminTable (getRows), toList, validateToken)
import Pages.Projects.Brainfuck (code)
import qualified Tables as T (Credentials (Credentials, EmptyCredentials), GuestbookEntry (EmptyGuestbook, GuestbookEntry), LeaderboardEntry (EmptyLeaderboard, LeaderboardEntry))
import Utils (getDefault, unpackBS)

import IHP.HSX.QQ (hsx)
import Text.Blaze.Html (Html)

import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.UUID (toString)
import Data.UUID.V4 (nextRandom)

import Network.HTTP.Types.Status (Status, status200, status400, status404, status500)
import Network.Wai (Request (pathInfo, requestMethod), getRequestBodyChunk)

import Crypto.Random (getRandomBytes)
import Data.Aeson (Value (String), decode, encode)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Password.Bcrypt (PasswordCheck (PasswordCheckFail, PasswordCheckSuccess), PasswordHash (PasswordHash), checkPassword, mkPassword)
import Data.Text (intercalate, pack, unpack, Text)
import Data.Text.Array (Array (ByteArray))
import Database.Persist (Entity (Entity, entityKey), Filter (Filter), FilterValue (FilterValue), PersistFilter (BackendSpecificFilter), insertEntity, selectList, PersistQueryWrite (deleteWhere), (==.), (=.), (>.), PersistField (toPersistValue), PersistStoreRead (get), PersistUniqueRead (getBy), PersistQueryRead (count))
import Database.Schema (EntityField (TokenName, UserName, VisitUuid, TokenToken, GuestbookEntryId, VisitId, SnakeId, UserId, TokenId, GuestbookEntryParentId, VisitTimestamp), GuestbookEntry (GuestbookEntry, guestbookEntryContent, guestbookEntryName, guestbookEntryParentId, guestbookEntryTimestamp), Snake (Snake), Token (Token, tokenToken), User (User, userName, userPassword), Visit (Visit), Key (GuestbookEntryKey), Unique (Username))
import Logger (info)
import Text.StringRandom (stringRandomIO)

import Data.Aeson.QQ (aesonQQ)
import Data.ByteString.UTF8 (ByteString)
import Data.List (find)
import Network.HTTP.Types (HeaderName)
import Text.Regex (matchRegex, mkRegex)
import System.IO (openFile, IOMode (ReadMode, WriteMode), hGetContents, hPutStr, hClose, writeFile)
import System.Directory (getDirectoryContents, removeFile)
import Settings (getEditorRoot)
import Tables (DatabaseDelete(DatabaseDelete, EmptyDatabaseDelete))
import State (getCookies, getStates, loggedIn, accessToken)
import Database.Persist.Sql (toSqlKey)

type Header = (HeaderName, ByteString)
type APIResponse = IO (Status, String, [Header])
type APIEndpoint = (String, Request -> APIResponse)
type APIRoute = (String, [APIEndpoint])

j2s :: Value -> String
j2s = unpackBS . toStrict . encode

defaultHeaders :: [Header]
defaultHeaders = [("Content-Type", "text/plain")]

jsonHeaders :: [Header]
jsonHeaders = [("Content-Type", "application/json")]

messageResponse :: String -> String
messageResponse value = j2s [aesonQQ|{
    "message":#{value}
}|]

(<!>) :: [APIRoute] -> String -> [APIEndpoint]
((method, value):xs) <!> target | method == target  = value
                                | otherwise         = xs <!> target
[] <!> _ = []

(<!!>) :: [APIEndpoint] -> String -> (Request -> APIResponse)
((regex, response):xs) <!!> target  | regex == target  = response
                                    | otherwise         = xs <!!> target
[] <!!> _ = \r -> return (status400, j2s [aesonQQ|{}|], jsonHeaders)

apiMap :: [APIRoute]
apiMap = [
    ("POST", [
        ("^/login(/|)$", \r -> do
            body <- getRequestBodyChunk r
            let credentials = getDefault T.EmptyCredentials (decode (fromStrict body) :: Maybe T.Credentials)
            case credentials of
                (T.Credentials username password) -> do
                    let pass = mkPassword $ pack password
                    maybeUser <- runDb $ getBy $ Username username
                    case maybeUser of
                        (Just (Entity id user)) -> case checkPassword pass (PasswordHash $ pack (userPassword user)) of
                            PasswordCheckSuccess -> do
                                token <- stringRandomIO "[0-9a-zA-Z]{4}-[0-9a-ZA-Z]{10}-[0-9a-zA-Z]{15}"
                                runDb $ insertEntity $ Token (unpack token) username
                                return (status200, j2s [aesonQQ|{"token":#{unpack token}}|], jsonHeaders)
                            PasswordCheckFail -> return (status400, messageResponse "Error, Wrong username or password", jsonHeaders)
                        _ -> return (status400, messageResponse "Error, no user exists", jsonHeaders)
                _ -> return (status400, messageResponse "Error, Invalid request", jsonHeaders)
        ),
        ("^/brainfuck(/|)$", \r -> do
            input <- getRequestBodyChunk r
            let result = code $ unpackBS input
            return (status200, result, [("Content-Disposition", "attachment; filename=\"brainfuck.c\"")])
        ),
        ("^/editor/new(/|)$", \r -> do
            filename <- getRequestBodyChunk r
            editor_root <- getEditorRoot
            files <- getDirectoryContents editor_root
            if unpackBS filename `elem` files then do
                return (status400, j2s [aesonQQ|{"message":"Error! file already exists"}|], jsonHeaders)
            else do
                handle <- openFile (editor_root ++ "/" ++ unpackBS filename) WriteMode
                hPutStr handle ""
                hClose handle
                return (status200, j2s [aesonQQ|{"status":"ok"}|], jsonHeaders)
        )
    ]),
    ("GET", [
        ("^(/|)$", \_ -> do
            let apiData = map (\(method, routes) -> [aesonQQ|{
                "method":#{method},
                "routes":#{map fst routes}
            }|]) apiMap
            return (status200, j2s [aesonQQ|#{apiData}|], jsonHeaders)
        ),
        ("^/visits/get(/|)$", \_ -> do
            visits <- runDb $ count [VisitTimestamp >. 0]
            return (status200, j2s [aesonQQ|{"visits":#{visits}}|], jsonHeaders)
        ),
        ("^/guestbook/get(/|)$", \_ -> do
            entries <- getRows [] [] :: IO [Entity GuestbookEntry]
            let l = map toList entries
            return (status200, j2s [aesonQQ|{"entries":#{l}}|], jsonHeaders)
        ),
        ("^/editor/sidebar(/|)$", \_ -> do
            editor_root <- getEditorRoot
            files <- getDirectoryContents editor_root
            return (status200, j2s [aesonQQ|#{files}|], jsonHeaders)
        ),
        ("^/editor/content/.*(/|)$", \r -> do
            let filename = unpack $ last $ pathInfo r
            editor_root <- getEditorRoot
            handle <- openFile (editor_root ++ "/" ++ filename) ReadMode
            contents  <- hGetContents handle
            return (status200, contents, defaultHeaders)
        )
    ]),
    ("PUT", [
        ("^/guestbook/add(/|)$", \r -> do
            body <- getRequestBodyChunk r
            let entry = getDefault T.EmptyGuestbook (decode (fromStrict body) :: Maybe T.GuestbookEntry)
            case entry of
                (T.GuestbookEntry "" _ _) -> 
                    return (status400, messageResponse "Error, name cannot be empty", jsonHeaders)
                (T.GuestbookEntry _ "" _) -> 
                    return (status400, messageResponse "Error, content cannot be empty", jsonHeaders)
                (T.GuestbookEntry name content parentId) -> do
                    time <- fmap round getPOSIXTime :: IO Int
                    runDb $ insertEntity $ GuestbookEntry time name content parentId
                    return (status200, messageResponse "Success", jsonHeaders)
                _ -> do
                    return (status500, messageResponse "Error, server failed", jsonHeaders)
        ),
        ("^/snake/add(/|)$", \r -> do
            body <- getRequestBodyChunk r
            let entry = getDefault T.EmptyLeaderboard (decode (fromStrict body) :: Maybe T.LeaderboardEntry)
            case entry of
                T.EmptyLeaderboard -> return (status400, messageResponse "Error, leaderboard empty", jsonHeaders)
                (T.LeaderboardEntry name score speed fruits) -> do
                    time <- fmap round getPOSIXTime :: IO Int
                    runDb $ insertEntity $ Snake time name score speed fruits
                    return (status200, messageResponse "Success", jsonHeaders)
        ),
        ("^/editor/content/.*(/|)$", \r -> do
            body <- getRequestBodyChunk r
            let content = unpackBS body
            let filename = unpack $ last $ pathInfo r
            editor_root <- getEditorRoot
            handle <- openFile (editor_root ++ "/" ++ filename) WriteMode
            hPutStr handle content
            hClose handle
            return (status200, messageResponse "ok", jsonHeaders)
        )
    ]),
    ("DELETE", [
        ("^/editor/delete(/|)$", \r -> do
            body <- getRequestBodyChunk r
            editor_root <- getEditorRoot
            let filename = unpackBS body
            removeFile $ editor_root ++ "/" ++ filename
            return (status200, messageResponse "ok", jsonHeaders)
        ),
        ("^/database/delete(/|)$", \r -> do
            body <- getRequestBodyChunk r
            let json = getDefault EmptyDatabaseDelete (decode (fromStrict body) :: Maybe DatabaseDelete)
            let states = getStates r

            if loggedIn states then do 
                validity <- validateToken (accessToken states)
                if validity then case json of
                    (DatabaseDelete table id) -> do
                        case table of
                            "visits" -> do
                                runDb $ deleteWhere [VisitId ==. (toSqlKey . read . show) id]
                            "guestbook" -> do
                                runDb $ deleteWhere [GuestbookEntryId ==. (toSqlKey . read . show) id]
                            "snake" -> do
                                runDb $ deleteWhere [SnakeId ==. (toSqlKey . read . show) id]
                            "users" -> do
                                runDb $ deleteWhere [UserId ==. (toSqlKey . read . show) id]
                            "valid_tokens" -> do
                                runDb $ deleteWhere [TokenId ==. (toSqlKey . read . show) id]
                            _ -> putStr "no table, doing nothing..."
                        return (status200, messageResponse "ok", jsonHeaders)
                    EmptyDatabaseDelete -> return (status400, messageResponse "Invalid JSON", jsonHeaders)
                else return (status400, messageResponse "Invalid token", jsonHeaders)
            else return (status400, messageResponse "Not logged in", jsonHeaders)
        )
    ])
    ]

api :: Request -> APIResponse
api request = do
    let method = unpackBS $ requestMethod request
    let path = pathInfo request
    
    case find (\(name, _) -> name == method) apiMap of
        (Just (_, endpoints)) -> case find (checkEndpoint path) endpoints of
            (Just (_, f)) -> f request
            Nothing -> return (status400, messageResponse "Error, no endpoint found", jsonHeaders)
        Nothing -> return (status400, messageResponse "Error, no endpoint found", jsonHeaders)
    where
        checkEndpoint path (regex, _) = case matchRegex (mkRegex regex) $ "/" ++ unpack (intercalate "/" (drop 1 path)) of
            Nothing -> False
            _ -> True
