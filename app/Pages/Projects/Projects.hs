module Pages.Projects.Projects where

import IHP.HSX.QQ (hsx)
import Text.Blaze.Html (Html)

import Data.List (find, intercalate, replicate)

import Pages.Projects.Brainfuck (brainfuck)
import Pages.Projects.Snake (snake)
import Section (section)
import Tree (Tree (..))

import CodeBlock (codeBlock)
import Database.Database (prettyPrintSchema)
import Database.Schema (defs)
import Layout (layout)
import Page (Page, PageSetting (Description, Route), getArgs)
import Pages.Projects.Editor (editor)
import Api.Api (apiMap)

defaultProject :: (String, Html)
defaultProject = ("", section [hsx|
    Use the sidebar to find a project :)<br><br>

    This page is inspired by my friend Mohamad, his site is available below<br><br>
    <a href="https://mohamaddalal.github.io/">Mohamad's site</a>
|])

projectsTree :: Tree (String, Html)
projectsTree = Tree defaultProject [
    Tree ("Semester Projects", mconcat $ section [hsx|
            Here's all the projects i've done at Aalborg University, they're defined as Pn where n is the semester they were done at. for example, P6 and P10 is my bachelor and master's projects respectively.
        |] : [mconcat [[hsx|<h3>{"P"++show i ++ " Project"}</h3>|], snd $ findItem ["", "Semester Projects", "P"++show i] projectsTree] | i <- [1..9]]
        
    ) [
        Tree ("P1", section [hsx|
            P1 was about Random Linear Network Coding, to send extra bytes coded together, and reduce the impact of packet loss
            <br>
            It was cool
        |])[], 
        Tree ("P2", section [hsx|
            A Project about adaptive cruise control in cars, this was written in python using the pygame library. 
        |]) [],
        Tree ("P3", section [hsx|
            We made a satellite ground station to be full duplex, as the previous implementation could only send data one way at a time, would be cool to use two channels.
        |]) [],
        Tree ("P4", section [hsx|
            Detecting fires on a map, it wasn't particularly interesting.<br>
            We learned how to use libraries like OpenCV and further sharpened knowledge about common python libraries like numpy
        |]) [],
        Tree ("P5", section [hsx|
            Testing TCP performance using NS3, we learned a bit of C++, it was nice.<br>
            There was some interesting plots of the congestion control algorithms and their performance, we didn't have a statistics course yet, but i think if we had, we could have analyzed the results much more accurately
        |]) [],
        Tree ("P6", section [hsx|
            Modeling a testbed for edge nodes for measurement in real world scenarios<br><br>

            It was a pretty interesting project, as we designed our own dataframe instead of using like HTTP, it made it very fast, but as could be read in our semester report, our system could be even faster if we optimized language and protocols. <br><br>
                
            It would probably be beyond our expectations if we went and implemented our own solution at the data-link layer of networking instead of at the routing layer (or whatever its called again in TCP/IP)
        |])[], 
        Tree ("P7", section [hsx|
            The semester we learned haskell! Honestly i think i spent more time in my free time in total on haskell than i did thinking about this project. The project was about measuring the amount of people in a room using IoT devices and bluetooth.<br><br>
                
            The coolest part of this project was definitely with fidding with low-level promisquous mode on an IoT device.
        |]) [], 
        Tree ("P8", section [hsx|
            This was a project about conducting a user-study, measuring people's stress and questioning them through an app on a mobile phone.
            <br>
            (this project was very, very bad imo, but i learned more C++)
        |]) [],
        Tree ("P9", section [hsx|
            This is my pre-specialization project, it is focused on distributed systems, and how to more efficiently schedule pods in Kubernetes.<br>

            In this project we're attempting to use machine learning to predict the traffic of a system, then scale the system up based on the predictions.
        |]) [],
        Tree ("P10", section [hsx|
            This is my master's thesis! In this project we spent the entire semester perfecting our machine learning pipeline, by implementing our own model selection system based on statistical data about our data based on backtesting with a validation set and the first portion of the predicted set.<br>
            An improvement to our paper would have been to document how we designed our model selection algorithmically, while our machine learning would also have benefitted from being based around an event-driven system like Apache Kafka, as we essentially used postgres as our event storage.
        |]) []
    ], Tree ("Personal Projects", section [hsx|
        I find it fun coding in my free time, i do it a lot and as such this website was also born!
    |]) [
        Tree ("Snake", snake) [], 
        Tree ("Brainfuck Transpiler", brainfuck) [],
        Tree ("Text Editor", editor) [],
        Tree ("Website", mconcat [
            section [hsx|
                <div style="max-width: 100%">
                    Written in Haskell using IHP-HSX as the primary library, and persistent-mysql as the database implementation.<br>

                    The database is actually pretty cool, its implemented as a list of table objects, and since i'm writing html directly inside my haskell code i can easily print the database structure inline here:<br><br>
                    {codeBlock "txt" $ show defs}
                    Its not particularly nice looking in this way, so i'll print it properly below :P
                    {codeBlock "txt" prettyPrintSchema}<br>
                    Still in haskell, but the beauty of haskell is that stuff like that is a short oneliner<br><br>

                    <span style="color: red">DISCLAIMER:</span> there might be extra tables in this list that shows up without a corresponding feature, i am actually generating it on the fly.

                    <br><br>
                    This page about projects is actually also pretty cool, its defined as a tree data structure, so i can also easily print it:
                    {codeBlock "haskell" $ show (Tree ("projects", "<html>") [Tree ("page2", "<html>") [], Tree ("page3", "<html>") [], Tree ("page4", "<html>") []])}
                    <h3>API</h3>
                    The api is likewise actually implemented as a list of pairs of strings and lists of pairs of strings and functions taking a request and returning a triple as a response.

                    <br>
                    Each endpoint is addressed using a string representing the method from the REST standard, and a route in the form of a regular expression (see more below).

                    <br>
                    Below the API will dynamically update the more endpoints i'll add.
                    {codeBlock "txt" $ intercalate "\n" $ map (\(method, routes) -> method ++ "\n\t" ++ (intercalate "\n\t" $ map fst routes)) apiMap}

                    <h2>Versions</h2>
                    In the sidebar, or below you can choose to read about each version of this website.
                </div>
            |], 
            [hsx|<h3>Version 1</h3>|],
            snd (findItem ["", "Personal Projects", "Website", "Version 1"] projectsTree),
            [hsx|<h3>Version 2</h3>|],
            snd (findItem ["", "Personal Projects", "Website", "Version 2"] projectsTree),
            [hsx|<h3>Version 3</h3>|],
            snd (findItem ["", "Personal Projects", "Website", "Version 3"] projectsTree),
            [hsx|<h3>Version 4</h3>|],
            snd (findItem ["", "Personal Projects", "Website", "Version 4"] projectsTree)
        ]) [
            Tree ("Version 1", section [hsx|
                Was written on github pages using markdown<br>
                barely had any content.
            |]) [],
            Tree ("Version 2", section [hsx|
                Was written in html, css and javascript, had a lot of client side javascript and is still available at <a href="https://about.skademaskinen.win">https://about.skademaskinen.win</a><br>
                The guestbook and the interests page was my main goal and i finished both of them.<br>
                Source code is available at <a href="https://github.com/Skademaskinen/Frontend">https://github.com/Skademaskinen/Frontend</a><br>
                <br>
                Additionally as there was non-static elements on this website, i needed an external backend that took http requests as well, this worked pretty well, and it circumvented that the server it was hosted on didn't have full port access. (it took HTTP on port 11034) with the actual website being served from github pages. The backend was written in python and the source code is available at <a href="https://github.com/Skademaskinen/Backend">https://github.com/Skademaskinen/Backend</a>
            |]) [],
            Tree ("Version 3", section [hsx|
                This was written in haskell using the full IHP framework, it was a lot of framework to code around compared to the older sites, ofc this made it possible to write more functionality with less code, but with such a feature also comes a lot of restrictions, such as the database being very hard to implement, and dependencies being less easily managed and coding an API using raw HTTP was very restrictive. hence version 4.<br>
                Source code is available at <a href="https://github.com/Skademaskinen/F3">https://github.com/Skademaskinen/F3</a>
            |]) [],
            Tree ("Version 4", section [hsx|
                This version is also written in haskell, but this time also using Warp directly to translate HSX to blaze and parse blaze to a bytestring. Its this current site and doesn't require a link :P<br>
                Source code is available at <a href="https://github.com/Mast3rwaf1z/homepage">https://github.com/Mast3rwaf1z/homepage</a>
                <br>
                The best feature of  this rewrite is that i've combined the backend and the frontend into one, and since HSX allows me to put IO code directly inline with my html, i can access the database directly instead of the frontend sending a million HTTP requests. This means that the website is A LOT faster than the first few iterations, and instead of a lot of content loading at different times, its all loaded at once at the server side, with as little javascript as possible.
            |]) []
        ],
        Tree ("Skademaskinen", section [hsx|
            This is about my server, it hosts a lot of things, but the things accessible from HTTP is available at:
            <br>
            <div style="text-align:center;">
                {services}
            </div>
        |]) [
            Tree ("Archerus", section [hsx|
                This is my collection of NixOS configuration. I will extend this constantly and contains NixOS configurations for all systems i run NixOS on. It has modules that can all be loaded individually or pulled in together.<br>
                The project is available <a href="https://github.com/Skademaskinen/Archerus">here</a><br>
                A set of configuration options are also defined, all are available <a href="https://docs.skade.dev/Options">here</a><br>
                Finally an architecture diagram is auto generated and also available on this server <a href="https://docs.skade.dev/Architecture">here</a>
            |]) []
        ],
        Tree ("Discord bots", section [hsx|
            I have written a few bots for discord, all of them were in Java using the dv8tion JDA (java bindings for the discord API). The first version was Mør Bot. It provided commands before slash commands that could play music from youtube and soundcloud, attached an emote to random messages, and showed a leaderboard for these emotes. It also allowed for setting roles.
            <br>
            <a href="https://github.com/Mast3rwaf1z/Moer-bot">https://github.com/Mast3rwaf1z/Moer-bot</a>
            <br><br>
            Next i rewrote it into a discord bot to manage my guild in World of Warcraft's raiding team, and manage roles for the discord server. It was fun but i also realized that releasing solo software for a bigger group of people (up to 100 people) could cause problems. This bot also reuses a lot of the code from Mør bot, while also used a lot more OOP patterns. It also employs a better role assigning system and uses a module loader to load command execution patterns on the fly, which was fun to code.
            <br>
            <a href="https://github.com/Skademaskinen/Putricide">https://github.com/Skademaskinen/Putricide</a>
            <br><br>
            The final bot was written as a side project to parse discord text channels to LaTeX files, compiling them to PDF and making them available for download.
            <br>
            Additionally it was written with a lot more tests and proper CI, to make sure it computes word counts and stuff correctly.
            <br>
            <a href="https://github.com/Skademaskinen/rp-utils">https://github.com/Skademaskinen/rp-utils</a>
        |]) [],
        Tree ("WoW Addon", section [hsx|
            I have written an Addon for World of Warcraft that is essentially an extension of TwitchEmotes.<br>
            It is up on CurseForge <a href="https://www.curseforge.com/wow/addons/twitchemotes-paradox">here</a>, it adds emotes specifically added on requests from my guild in World of Warcraft. 
        |]) []
    ]
    ]

services :: Html
services = mconcat $ map (\(name, d) -> [hsx|
    <button id={d} onclick="go_to(this.id)">{name}</button>
|]) srv
    where
        srv :: [(String, String)]
        srv = [("Nextcloud", "cloud"), ("Jupyter", "jupyter"), ("Matrix", "matrix"), ("Website", "api"), ("Taoshi", "taoshi")]

makeIndent :: Int -> Html
makeIndent n = mconcat $ map (\_ -> [hsx|&emsp;|]) [0..n]

buildSidebarSection :: [Tree (String, Html)] -> Int -> String -> Html
buildSidebarSection (x : xs) indent path = [hsx|
    {buildSidebarItem x indent path}
    {buildSidebarSection xs indent path}
|]
buildSidebarSection [] _ _ = [hsx||]

buildSidebarItem :: Tree (String, Html) -> Int -> String -> Html
buildSidebarItem (Tree (name, _) children) indent path = [hsx|
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
findItem_ (target : xs) (Tree value children) = do
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

page target = [hsx|
    <div style="width=100%;">
        {mainView target}
    </div>
|]

settings :: [PageSetting]
settings = [
    Route "/projects",
    Description "List of all projects i have done"
    ]

projects :: Page
projects = (settings, \req -> do
    let (_ : project) = getArgs req
    return $ layout $ page project
    )
