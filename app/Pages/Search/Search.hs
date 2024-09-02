module Pages.Search.Search where
import Text.Blaze.Html (Html)
import IHP.HSX.QQ (hsx)
import Text.Regex (Regex, matchRegex)
import Network.Wai (Request, Response)
import Helpers.Section (section)

findPage :: [(String, Request -> IO Response)] -> Regex -> [String]
findPage ((target, _):xs) regex = case matchRegex regex target of
    Nothing -> findPage xs regex
    _ -> target : findPage xs regex
findPage [] _ = []

search :: [(String, Request -> IO Response)] -> Regex -> Html
search pages query = [hsx|
    <h1>Search Results:</h1>
    {section $ makeResults 0 $ findPage pages query}
|]
    where
        makeResults :: Int -> [String] -> Html
        makeResults index (x:xs) = [hsx|
            <h2>{index} - {title}</h2>
            <a href={x}>{x}</a>
            <br>
            {makeResults (index+1) xs}
        |]
            where
                (_:title) = x
        makeResults _ [] = [hsx||]