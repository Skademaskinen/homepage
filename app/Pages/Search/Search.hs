module Pages.Search.Search where
import Text.Blaze.Html (Html)
import IHP.HSX.QQ (hsx)
import Text.Regex (Regex, matchRegex, mkRegex)
import Network.Wai (Request, Response)
import Helpers.Section (section)

type Page = (String, String, Request -> IO Html)

findPage :: [Page] -> Regex -> [(String, String)]
findPage ((target, description, _):xs) regex = case matchRegex regex target of
    Nothing -> findPage xs regex
    _ -> (target, description) : findPage xs regex
findPage [] _ = []

search :: [Page] -> String -> Html
search pages query = [hsx|
    <h1>Search Results: [query={query}]</h1>
    {section $ makeResults 1 $ findPage pages $ mkRegex query}
|]
    where
        makeResults :: Int -> [(String, String)] -> Html
        makeResults index ((x, description):xs) = [hsx|
            <h2>{index} - {title}</h2>
            <a href={x}>{x}</a>
            <p>{description}</p>
            <br>
            {makeResults (index+1) xs}
        |]
            where
                (_:title) = x
        makeResults _ [] = [hsx||]