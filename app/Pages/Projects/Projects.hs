module Pages.Projects.Projects where

import IHP.HSX.QQ
import Text.Blaze.Html

import Data.List

projects_list :: [(String, Html)]
projects_list = [
    ("P1", [hsx|
        P1 was about Random Linear Network Coding
        <br>
        It was cool
    |]),
    ("P2", [hsx|
        A Project was about cruise control
    |]),
    ("Snake", [hsx|
        Subproject of this page, and its all contained here!
        <br>
        I taught myself javascript...
    |])]

mapHtml :: (a -> Html) -> [a] -> Html
mapHtml f [] = [hsx||]
mapHtml f (x:xs) = [hsx|
    {f x}
    <br>
    {mapHtml f xs}
|]

sidebar_entries :: Maybe String -> Html
sidebar_entries (Just selected) = mapHtml (\(name,_) -> [hsx|
    <a href={"/projects/"++name}>{(if name == selected then "> " else "") ++ name}</a>
|]) projects_list
sidebar_entries Nothing = mapHtml (\(name,_) -> [hsx|
    <a href={"/projects/"++name}>{name}</a>
|]) projects_list

sidebar :: Maybe String -> Html
sidebar name = [hsx|
    <div style="float: left; width: 300px; background-color: #111111; border: 1px solid var(--main-color); border-radius: 5px;">
        {sidebar_entries name}
    </div>
|]

mainView :: Maybe (String, Html) -> Html
mainView (Nothing) = [hsx|
    {sidebar Nothing}
    <div style="margin-left: 320px; min-height: 1000px;">
        Select something in the sidebar :)
    </div>
|]
mainView (Just (name,project)) = [hsx|
    {sidebar (Just name)}
    <div style="margin-left: 320px; min-height: 1000px;">
        {project}
    </div>
|]

projects :: String -> Html
projects target = [hsx|
    <div style="width=100%;">
        {mainView (find (\(name, html) -> name == target) projects_list)}
    </div>
|]