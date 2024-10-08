module Index where

import IHP.HSX.QQ (hsx)
import Text.Blaze.Html (Html)

import Helpers.CodeBlock (hsxIntroCodeBlock, introCodeIndex)
import Helpers.Section (section)
import Helpers.Database.Database (getVisits)
import Layout (layout)
import Helpers.Page (PageSetting(Description, Route, EmbedImage, EmbedText), Page)

intro :: Html
intro = section [hsx|
    Welcome to my home page, returning visitors might notice that the same layout that i always use is still present, and yes the site is still written in Haskell.
    <br><br>
    I was using the whole IHP development suite to work on the site originally, but i have a problem with that framework: its huge and very overkill for what i'm trying to do.
    <br><br>
    Therefore i've taken to just use pure HSX (XML syntax within haskell code) and just a little bit of wrappers around sqlite to manage the non-static elements.
    <br><br>
    I know i know its like my 4th rewrite of this website, but i really, really wanted to get rid of python, but IHP is also a bit too big a bite for me at the moment.
    <br><br>
    So here we are, i've sorted out like the different pages under the app directory, and made like a static directory filled with static files.
    <br><br>
    The project is managed by cabal, a little build tool i've had to learn myself as i had only gotten experience with pure haskell without libraries from uni.
    <br><br>
    Anyways...
    <br><br>
    My name is Thomas Jensen, i'm a graduate student at Aalborg University. I study Software Engineering with a bachelor degree in Computer Engineering.
    <br><br>
    HSX is actually pretty cool, i just toss in html inline with haskell and it just works: 
    <br><br>
    {hsxIntroCodeBlock}
    I had to do a little hack around the preprocessor to make it not compile that little snippet of code, and as such you won't see me show any more hsx code xD<br>
    Running the above code:
    <br>
    <div style="background-color: #222222">
        {introCodeIndex}
    </div>
|]


page :: IO Html
page = do 
    visits <- show . length <$> getVisits
    return [hsx|
    <h1>Skademaskinen</h1>
    <img src="/static/icon.png" style="border-radius:50%">
    <br>
    <hr>
    {intro}
    Visitors: <p id="visits">{visits}</p>
    <script>
        fetch("/api/visits/new", {
            method: "post",
            body: getCookie("visitId")
        }).then(res => res.text().then(uuid => setCookie("visitId="+uuid)))
    </script>
|]

settings :: [PageSetting]
settings = [
    Route "/",
    Description "This is the front page for Skademaskinen, a server built as a passion project.",
    EmbedImage "/static/icon.png",
    EmbedText "Skademaskinen - Index"
    ]


index :: Page
index = (settings, const $ layout <$> page)