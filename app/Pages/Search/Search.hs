module Pages.Search.Search where

import IHP.HSX.QQ (hsx)
import Layout (layout)
import Network.Wai (Request, Response)
import Page (Page, PageSetting (Description, Route), description, getArgs, route)
import Section (section)
import Text.Blaze.Html (Html)
import Text.Regex (Regex, matchRegex, mkRegex)

findPage :: [Page] -> Regex -> [(String, String)]
findPage ((settings, _) : xs) regex = case matchRegex regex (route settings) of
  Nothing -> findPage xs regex
  _ -> (route settings, description settings) : findPage xs regex
findPage [] _ = []

page pages query =
  [hsx|
    <h1>Search Results: [query={query}]</h1>
    {section $ makeResults 1 $ findPage pages $ mkRegex query}
|]
 where
  makeResults :: Int -> [(String, String)] -> Html
  makeResults index ((x, description) : xs) =
    [hsx|
            <h2>{index} - {title}</h2>
            <a href={x}>{x}</a>
            <p>{description}</p>
            <br>
            {makeResults (index+1) xs}
        |]
   where
    (_ : title) = x
  makeResults _ [] = [hsx||]

settings :: [PageSetting]
settings =
  [ Route "/search"
  , Description "Search for pages"
  ]

search :: [Page] -> Page
search pages =
  ( settings
  , \req -> do
      let [_, query] = getArgs req
      return $ layout $ page pages query
  )
