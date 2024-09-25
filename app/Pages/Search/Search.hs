module Pages.Search.Search where
import Text.Blaze.Html (Html)
import IHP.HSX.QQ (hsx)
import Text.Regex (Regex, matchRegex, mkRegex)
import Network.Wai (Request, Response)
import Helpers.Section (section)
import Helpers.Page (Page, description, route, getArgs, PageSetting (Route, Description))
import Layout (layout)


findPage :: [Page] -> Regex -> [(String, String)]
findPage ((settings, _):xs) regex = case matchRegex regex (route settings) of
    Nothing -> findPage xs regex
    _ -> (route settings, description settings) : findPage xs regex
findPage [] _ = []


page pages query = [hsx|
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

settings :: [PageSetting]
settings = [
    Route "/search", 
    Description "Search for pages"
    ]

search :: [Page] -> Page
search pages = (settings, \req -> do 
    let [_, query] = getArgs req
    return $ layout $ page pages query)