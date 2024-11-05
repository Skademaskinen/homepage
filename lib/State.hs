module State where
import Network.Wai (Request (requestHeaders))
import Data.List.Split (splitOn)
import Utils (unpackBS)
import Data.List (find)

data State = EmptyState
           | LoginState String
    deriving Show

getCookies :: Request -> [(String, String)]
getCookies request = map (\(_, cookie) -> (head $ splitOn "="  (unpackBS cookie), splitOn "=" (unpackBS cookie) !! 1)) $ filter (\(header, value) -> header == "Cookie") $ requestHeaders request

accessToken :: [State] -> String
accessToken states = case find (\case
    EmptyState -> False
    (LoginState _) -> True) states of
        Nothing -> ""
        (Just (LoginState value)) -> value

loggedIn :: [State] -> Bool
loggedIn = any (\case
    LoginState _ -> True
    _ -> False)

getStates :: Request -> [State]
getStates request = filterEmpty [
    case find (\(name, _) -> name == "accessToken") cookies of 
        (Just (_, value)) -> LoginState value
        Nothing -> EmptyState
    ]
    where
        cookies = getCookies request
        filterEmpty :: [State] -> [State]
        filterEmpty = filter (\case
            EmptyState -> False
            _ -> True
            )
