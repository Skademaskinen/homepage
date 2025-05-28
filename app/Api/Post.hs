module Api.Post where
import Api.Types (APIEndpoint, j2s, messageResponse, jsonHeaders)
import Database.Schema (User(User, userPassword), Unique (Username), Token (Token))
import Database.Persist (Entity(Entity), PersistUniqueRead (getBy), PersistStoreWrite (insert))
import Data.Password.Bcrypt (PasswordCheck(PasswordCheckSuccess, PasswordCheckFail), checkPassword, mkPassword, PasswordHash (PasswordHash))
import Data.Aeson.QQ (aesonQQ)
import Network.Wai (getRequestBodyChunk)
import Data.Aeson (decode)
import Data.ByteString (fromStrict)
import Data.Text (pack, unpack)
import Database.Database (runDb)
import Text.StringRandom (stringRandomIO)
import Network.HTTP.Types (status200, status400)
import Pages.Projects.Brainfuck (code)
import Utils (unpackBS)
import Settings (getEditorRoot)
import System.Directory (getDirectoryContents)
import System.IO (openFile, hPutStr, hClose, IOMode (WriteMode))

postMap :: [APIEndpoint]
postMap = [
    ("^/login(/|)$", \r -> do
        body <- getRequestBodyChunk r
        case decode $ fromStrict body of
            (Just (User username password)) -> do
                let pass = mkPassword $ pack password
                maybeUser <- runDb $ getBy $ Username username
                case maybeUser of
                    (Just (Entity id user)) -> case checkPassword pass (PasswordHash $ pack (userPassword user)) of
                        PasswordCheckSuccess -> do
                            token <- stringRandomIO "[0-9a-zA-Z]{4}-[0-9a-ZA-Z]{10}-[0-9a-zA-Z]{15}"
                            runDb $ insert $ Token (unpack token) username
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
    ]
