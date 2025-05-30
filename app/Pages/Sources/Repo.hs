{-# LANGUAGE OverloadedStrings #-}

module Pages.Sources.Repo where

import Data.Aeson (Array, FromJSON, Object, Value (Object), decode, (.:))
import qualified Data.ByteString.Char8 as S8
import IHP.HSX.QQ (hsx)
import Network.HTTP.Client.Conduit (Request (requestHeaders), method)
import Network.HTTP.Conduit (http, parseRequest, simpleHttp)
import Network.HTTP.Simple (getResponseBody, httpJSON, httpLBS, setRequestHeader)
import Text.Blaze.Html (Html)

import Control.Applicative
import Data.Text (Text)
import qualified Data.Yaml as Yaml

newtype Author = Author { date :: Text }

data Commit = Commit { message :: Text, author :: Author }

data CommitData = CommitData { url :: Text, commit :: Commit }

instance FromJSON CommitData where
    parseJSON (Object v) = CommitData
        <$> v .: "html_url"
        <*> v .: "commit"
    parseJSON _ = empty

instance FromJSON Commit where
    parseJSON (Object v) = Commit
        <$> v .: "message"
        <*> v .: "author"
    parseJSON _ = empty

instance FromJSON Author where
    parseJSON (Object v) = Author
        <$> v .: "date"
    parseJSON _ = empty

get :: IO [CommitData]
get = do
    initialRequest <- parseRequest "https://api.github.com/repos/Skademaskinen/homepage/commits"
    let request = initialRequest{method = "GET", requestHeaders = [("User-Agent", "Website")]}
    response <- httpJSON request
    return (getResponseBody response :: [CommitData])

repo :: IO Html
repo = do
    result <- handleCommits <$> get
    return [hsx|
        <hr>
        Commit History:<br>
        <table class="common-table">
        {result}
        </table>
    |]
    where
        handleCommits ((CommitData url (Commit message (Author date))) : xs) = [hsx|
            <tr>
                <td class="common-table-element">
                    <a href={url}>{message}</a>
                </td>
                <td class="common-table-element">
                    {date}
                </td>
            </tr>
            {handleCommits xs}
        |]
        handleCommits [] = [hsx||]
