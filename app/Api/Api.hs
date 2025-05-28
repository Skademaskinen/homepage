{-# LANGUAGE DataKinds #-}

module Api.Api where
import Api.Types (APIRoute, APIResponse, messageResponse, jsonHeaders)
import Api.Post (postMap)
import Api.Get (getMap)
import Api.Put (putMap)
import Api.Delete (deleteMap)
import Utils (unpackBS)
import Network.Wai (Request (requestMethod, pathInfo))
import Data.List (find, intercalate)
import Network.HTTP.Types (status400)
import Text.Regex (matchRegex, mkRegex)
import Data.Text (unpack)


apiMap :: [APIRoute]
apiMap = [
    ("POST", postMap),
    ("GET", getMap apiMap),
    ("PUT", putMap),
    ("DELETE", deleteMap)
    ]

api :: Request -> APIResponse
api request = do
    let method = unpackBS $ requestMethod request
    let path = pathInfo request
    
    case find (\(name, _) -> name == method) apiMap of
        (Just (_, endpoints)) -> case find (checkEndpoint $ map unpack path) endpoints of
            (Just (_, f)) -> f request
            Nothing -> return (status400, messageResponse "Error, no endpoint found", jsonHeaders)
        Nothing -> return (status400, messageResponse "Error, no endpoint found", jsonHeaders)
    where
        checkEndpoint path (regex, _) = case matchRegex (mkRegex regex) $ "/" ++ intercalate "/" (drop 1 path) of
            Nothing -> False
            _ -> True
