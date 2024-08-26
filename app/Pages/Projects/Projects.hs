module Pages.Projects.Projects where

import IHP.HSX.QQ (hsx)
import Text.Blaze.Html (Html)

import Data.List (replicate, intercalate, find)

import Helpers.Tree ( Tree(..) )
import Helpers.Utils (forEach)
import Pages.Projects.Snake (snake)

import Helpers.Database (schema, prettyPrintSchema)
import Helpers.CodeBlock (codeBlock)

defaultProject :: (String, Html)
defaultProject = ("", [hsx|
    Use the sidebar to find a project :)<br><br>

    This page is inspired by my friend Mohamad, his site is available below<br><br>
    <a href="https://mohamaddalal.github.io/">Mohamad's site</a>
|])

projectsTree :: Tree (String, Html)
projectsTree = Tree defaultProject [
    Tree ("Semester Projects", [hsx|
        Here's all the projects i've done at Aalborg University, they're defined as Pn where n is the semester they were done at. for example, P6 and P8 is my bachelor and master's projects respectively.
    |]) [
        Tree ("P1", [hsx|
            <div class="section">
                P1 was about Random Linear Network Coding
                <br>
                It was cool
            </div>
        |]) [],
        Tree ("P2", [hsx|
            <div class="section">
                A Project about adaptive cruise control in cars
            </div>
        |]) [],
        Tree ("P3", [hsx|
            <div class="section">
                We made a satellite ground station to be full duplex, as the previous implementation could only send data one way at a time, would be cool to use two channels.
            </div>
        |]) [],
        Tree ("P4", [hsx|
            <div class="section">
                Detecting fires on a map, it wasn't particularly interesting.
            </div>
        |]) [],
        Tree ("P5", [hsx|
            <div class="section">
            Testing TCP performance using NS3, we learned a bit of C++, it was nice.
            </div>
        |]) [],
        Tree ("P6", [hsx|
            <div class="section">
                Modeling a testbed for edge nodes for measurement in real world scenarios<br><br>

                It was a pretty interesting project, as we designed our own dataframe instead of using like HTTP, it made it very fast, but as could be read in our semester report, our system could be even faster if we optimized language and protocols. <br><br>
                
                It would probably be beyond our expectations if we went and implemented our own solution at the data-link layer of networking instead of at the routing layer (or whatever its called again in TCP/IP)
            </div>
        |]) [],
        Tree ("P7", [hsx|
            <div class="section">
                The semester we learned haskell! Honestly i think i spent more time in my free time in total on haskell than i did thinking about this project. The project was about measuring the amount of people in a room using IoT devices and bluetooth.<br><br>
                
                The coolest part of this project was definitely with fidding with low-level promisquous mode on an IoT device.
            </div>
        |]) [],
        Tree ("P8", [hsx|
            <div class="section">
                This was a project about conducting a user-study, measuring people's stress and questioning them through an app on a mobile phone.
                <br>
                (this project was very, very bad imo, but i learned more C++)
            </div>
        |]) []
    ],
    Tree ("Personal Projects", [hsx|
        <div class="section">
            I find it fun coding in my free time, i do it a lot and as such this website was also born!
        </div>
    |]) [
        Tree ("Snake", snake) [],
        Tree ("Website", [hsx|
            <div class="section" style="max-width: 100%">
                Written in Haskell using IHP-HSX as the primary library, and sqlite-simple as the database implementation.<br>

                The database is actually pretty cool, its implemented as a list of table objects, and since i'm writing html directly inside my haskell code i can easily print the database structure inline here:<br><br>
                {codeBlock "txt" $ show schema}
                Its not particularly nice looking in this way, so i'll print it properly below :P
                {codeBlock "txt" prettyPrintSchema}<br>
                Still in haskell, but the beauty of haskell is that stuff like that is a short oneliner<br><br>

                <span style="color: red">DISCLAIMER:</span> there might be extra tables in this list that shows up without a corresponding feature, i am actually generating it on the fly.

                <br><br>
                This page about projects is actually also pretty cool, its defined as a tree data structure, so i can also easily print it:
                {codeBlock "haskell" $ show (Tree ("projects", "<html>") [Tree ("page2", "<html>") [], Tree ("page3", "<html>") [], Tree ("page4", "<html>") []])}
            </div>
        |]) [
            Tree ("Version 1", [hsx|
                <div class="section">
                    Was written on github pages using markdown<br>
                    barely had any content.
                </div>
            |]) [],
            Tree ("Version 2", [hsx|
                <div class="section">
                    Was written in html, css and javascript, had a lot of client side javascript and is still available at <a href="https://about.skademaskinen.win">https://about.skademaskinen.win</a><br>
                    The guestbook and the interests page was my main goal and i finished both of them.<br>
                    Source code is available at <a href="https://github.com/Skademaskinen/Frontend">https://github.com/Skademaskinen/Frontend</a><br>
                </div>
            |]) [],
            Tree ("Version 3", [hsx|
                <div class="section">
                    This was written in haskell using the full IHP framework, it was a lot of framework to code around compared to the older sites, ofc this made it possible to write more functionality with less code, but with such a feature also comes a lot of restrictions, such as the database being very hard to implement, and dependencies being less easily managed and coding an API using raw HTTP was very restrictive. hence version 4.<br>
                    Source code is available at <a href="https://github.com/Skademaskinen/F3">https://github.com/Skademaskinen/F3</a>
                </div>
            |]) [],
            Tree ("Version 4", [hsx|
                <div class="section">
                    This version is also written in haskell, but this time also using Warp directly to translate HSX to blaze and parse blaze to a bytestring. Its this current site and doesn't require a link :P<br>
                    Source code is available at <a href="https://github.com/Mast3rwaf1z/homepage">https://github.com/Mast3rwaf1z/homepage</a>
                </div>
            |]) []
        ],
        Tree ("Skademaskinen", [hsx|
            <div class="section">
                This is about my server, it hosts a lot of things, but the things accessible from HTTP is available at:
                <br>
                <div style="text-align:center;">
                {services}
                </div>
            </div>
        |]) []]]

services :: Html
services = mconcat $ map (\(name, d) -> [hsx|
    <button id={d} onclick="go_to(this.id)">{name}</button>
|]) srv
    where
        srv :: [(String, String)]
        srv = [("Nextcloud", "cloud"), ("Jupyter", "jupyter"), ("Matrix", "matrix"), ("Website", "api"), ("Taoshi", "taoshi")]

makeIndent :: Int -> Html
makeIndent n = forEach [0..n] (\_ -> [hsx|&emsp;|])

buildSidebarSection :: [Tree (String, Html)] -> Int -> String -> Html
buildSidebarSection (x:xs) indent path = [hsx|
    {buildSidebarItem x indent path}
    {buildSidebarSection xs indent path}
|]
buildSidebarSection [] _ _ = [hsx||]

buildSidebarItem :: Tree (String, Html) -> Int -> String -> Html
buildSidebarItem (Tree (name,_) children) indent path = [hsx|
    {makeIndent indent}<a href={"/projects/"++path++name}>{name}</a>
    <br>
    {buildSidebarSection children (indent+1) (path++name++"/")}
|]

sidebar :: Html
sidebar = [hsx|
    <div style="background-color: #111111; border: 1px solid #ff5500; border-radius: 5px;text-align: left;">
        {buildSidebarItem projectsTree (-1) ""}
        <br>
    </div>
|]

findItem_ :: [String] -> Tree (String, Html) -> Tree (String, Html)
findItem_ [] _ = return defaultProject
findItem_ [target] tree = do
    (name, html) <- tree
    if name == target then
        return (name, html)
    else
        return defaultProject
findItem_ (target:xs) (Tree value children) = do
    (name, html) <- Tree value children
    if name == target then
        case find (\(x, _) -> x /= fst defaultProject) $ map (findItem xs) children of
            Nothing -> return defaultProject
            (Just result) -> return result
    else
        return defaultProject

findItem :: [String] -> Tree (String, Html) -> (String, Html)
findItem target tree = result
    where
        (Tree result _) = findItem_ target tree


mainView :: [String] -> Html
mainView target = [hsx|
    <table style="width: 100%;">
        <tr>
            <th style="width:15%; text-align: left; vertical-align: top;line-height:30px;">{sidebar}</th>
            <th style="width:85%; max-width: 0px; vertical-align: top;">
                <h1>{title}</h1>
                {content}
            </th>
        </tr>
    </table>
|]
    where
        (title, content) = findItem target projectsTree

projects :: [String] -> Html
projects target = [hsx|
    <div style="width=100%;">
        {mainView target}
    </div>
|]
