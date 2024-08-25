module Pages.Projects.Projects where

import IHP.HSX.QQ
import Text.Blaze.Html

import Data.List

import Helpers.Tree
import Pages.Projects.Snake

projects_tree :: Tree (String, Html)
projects_tree = Tree ("", [hsx||]) [
    Tree ("Semester Projects", [hsx||]) [
        Tree ("P1", [hsx|
            P1 was about Random Linear Network Coding
            <br>
            It was cool
        |]) [],
        Tree ("P2", [hsx|
            A Project was about cruise control
        |]) []
    ],
    Tree ("Personal Projects", [hsx||]) [
        Tree ("Snake", snake)[]]]

make_indent :: Int -> String
make_indent n = intercalate "" $ replicate 3 " "

build_sidebar_section :: [Tree (String, Html)] -> Int -> String -> Html
build_sidebar_section (x:xs) indent path = [hsx|
    {build_sidebar_item x indent path}
    <br>
    {build_sidebar_section xs indent path}
|]
build_sidebar_section [] _ _ = [hsx||]

build_sidebar_item :: Tree (String, Html) -> Int -> String -> Html
build_sidebar_item (Tree (name,_) children) indent path = [hsx|
    {make_indent indent}<a href={"/projects/"++path++name}>{name}</a>
    <br>
    {build_sidebar_section children (indent+1) (path++name++"/")}
|]

sidebar :: Html
sidebar = [hsx|
    <div style="float: left; width: 15%; text-align: left; background-color: #111111; border: 1px solid #ff5500; border-radius: 5px;">
        <pre>
            <code>
                {build_sidebar_item projects_tree 0 ""}
            </code>
        </pre>
    </div>
|]

find_correct_section :: [String] -> [Tree (String, Html)] -> (String, Html)
find_correct_section [x] ((Tree (name, html) children'):children) | name == x = (name, html)
                                                                  | otherwise = find_correct_section [x] children
find_correct_section [x] [] = default_project
find_correct_section (x:xs) ((Tree (name, html) children'):children) | name == x = find_correct_section xs children'
                                                                     | otherwise = find_correct_section (x:xs) children
find_correct_section [] _ = default_project

find_correct_path :: [String] -> Tree (String, Html) -> (String, Html)
find_correct_path [x] (Tree (name, html) children) | name == x = (name, html)
                                                   | otherwise = default_project
find_correct_path (x:xs) (Tree (name, html) children) | name == x = find_correct_section xs children
                                                      | otherwise = default_project
find_correct_path [] _ = default_project


default_project :: (String, Html)
default_project = ("", [hsx|
    Use the sidebar to find a project :)
|])

mainView :: [String] -> Html
mainView target = [hsx|
    {sidebar}
    <div style="width: 85%; min-height: 1000px;">
        <h1>{title}</h1>
        {content}
    </div>
|]
    where
        (title, content) = find_correct_path target projects_tree

projects :: [String] -> Html
projects target = [hsx|
    <div style="width=100%;">
        {mainView target}
    </div>
|]