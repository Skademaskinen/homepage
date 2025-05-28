{-# LANGUAGE OverloadedStrings #-}

module Api.Put where
import Api.Types (APIEndpoint, messageResponse, jsonHeaders)
import Network.Wai (getRequestBodyChunk, Request (pathInfo))
import Utils (getDefault, unpackBS)
import Data.Aeson (decode)
import Data.ByteString (fromStrict)
import Data.Text (unpack)
import Network.HTTP.Types (status400, status200, status500)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Database.Database (runDb)
import Database.Persist (insertEntity, PersistStoreWrite (insert))
import Database.Schema (GuestbookEntry(GuestbookEntry), Snake (Snake), Member (Member))
import Settings (getEditorRoot)
import System.IO (IOMode(WriteMode), openFile, hPutStr, hClose)

putMap :: [APIEndpoint]
putMap = [
    ("^/guestbook/add(/|)$", \r -> do
        body <- getRequestBodyChunk r
        case decode $ fromStrict body of
            (Just (GuestbookEntry _ "" _ _)) -> 
                return (status400, messageResponse "Error, name cannot be empty", jsonHeaders)
            (Just (GuestbookEntry _ _ "" _)) -> 
                return (status400, messageResponse "Error, content cannot be empty", jsonHeaders)
            (Just (GuestbookEntry time name content parentId)) -> do
                runDb $ insert $ GuestbookEntry time name content parentId
                return (status200, messageResponse "Success", jsonHeaders)
            _ -> do
                return (status500, messageResponse "Error, server failed", jsonHeaders)
    ),
    ("^/snake/add(/|)$", \r -> do
        body <- getRequestBodyChunk r
        case decode $ fromStrict body :: Maybe Snake of
            Nothing -> return (status400, messageResponse "Error, leaderboard empty", jsonHeaders)
            (Just entry) -> do
                runDb $ insert entry
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
    ),
    ("^/folkevognen/add$", \r -> do
        body <- getRequestBodyChunk r
        let name = unpackBS body
        runDb . insert $ Member name
        return (status200, messageResponse "ok", jsonHeaders)
    )
    ]
