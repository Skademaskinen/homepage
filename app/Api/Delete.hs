module Api.Delete where
import Api.Types (APIEndpoint, messageResponse, jsonHeaders)
import Data.Aeson (FromJSON (parseJSON), Value (Object), (.:), decode)
import Control.Applicative (Alternative(empty))
import Network.Wai (getRequestBodyChunk)
import Settings (getEditorRoot)
import Utils (unpackBS)
import System.Directory (removeFile)
import Network.HTTP.Types (status200, status400)
import State (getStates, loggedIn, accessToken)
import Data.ByteString (fromStrict)
import Database.Database (validateToken, runDb)
import Database.Persist (PersistQueryWrite(deleteWhere), (==.))
import Database.Schema (EntityField(VisitId, GuestbookEntryId, SnakeId, UserId, TokenId))
import Database.Persist.MySQL (toSqlKey)

data DatabaseDelete = DatabaseDelete String Int | EmptyDatabaseDelete
instance FromJSON DatabaseDelete where
    parseJSON (Object v) = DatabaseDelete
        <$> v .: "table"
        <*> v .: "id"
    parseJSON _ = empty

deleteMap :: [APIEndpoint]
deleteMap = [
    ("^/editor/delete(/|)$", \r -> do
        body <- getRequestBodyChunk r
        editor_root <- getEditorRoot
        let filename = unpackBS body
        removeFile $ editor_root ++ "/" ++ filename
        return (status200, messageResponse "ok", jsonHeaders)
    ),
    ("^/database/delete(/|)$", \r -> do
        body <- getRequestBodyChunk r
        let states = getStates r

        if loggedIn states then do 
            validity <- validateToken (accessToken states)
            if validity then case decode $ fromStrict body of
                (Just (DatabaseDelete table id)) -> do
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
                _ -> return (status400, messageResponse "Invalid JSON", jsonHeaders)
            else return (status400, messageResponse "Invalid token", jsonHeaders)
        else return (status400, messageResponse "Not logged in", jsonHeaders)
    )
    ]
