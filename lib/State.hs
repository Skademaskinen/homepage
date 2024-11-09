module State where
import Network.Wai (Request (requestHeaders))
import Data.List.Split (splitOn)
import Utils (unpackBS)
import Data.List (find)

data State = EmptyState
           | LoginState String
           | VisitState String
    deriving Show

getCookies :: Request -> [(String, String)]
getCookies request = do
    let headers = requestHeaders request
    let cookies = (case find (\x -> ((==) . fst) x "Cookie") headers of (Just x) -> snd x; _ -> "")
    if cookies == "" then []
    else map ((\xs -> (xs !! 0, xs !! 1)) . splitOn "=") (splitOn "; " $ unpackBS cookies)


accessToken :: [State] -> String
accessToken ((LoginState value):xs) = value
accessToken (_:xs) = accessToken xs
accessToken [] = ""

visitId :: [State] -> String
visitId ((VisitState value):xs) = value
visitId (_:xs) = visitId xs
visitId [] = ""

loggedIn :: [State] -> Bool
loggedIn = any (\case
    LoginState _ -> True
    _ -> False)

getStates :: Request -> [State]
getStates request = filterEmpty [
    case find (\(name, _) -> name == "accessToken") cookies of 
        (Just (_, value)) -> LoginState value
        Nothing -> EmptyState,
    case find (\(name, _) -> name == "visitId") cookies of
        (Just (_, value)) -> VisitState value
        Nothing -> EmptyState
    ]
    where
        cookies = getCookies request
        filterEmpty :: [State] -> [State]
        filterEmpty = filter (\case
            EmptyState -> False
            _ -> True
            )
