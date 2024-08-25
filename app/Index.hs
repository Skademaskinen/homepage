module Index where

import IHP.HSX.QQ
import Text.Blaze.Html

import Helpers.CodeBlock
import Helpers.Section

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
    {codeBlock "haskell" "view :: Html\nview = [hsx\\|\n     <p>Hello World!</p>\n\\|\\]"}
    Ignore the backslashes, they're required as the html is rendered at compile time by GHC itself, so writing this code block was a little challenge
|]

visitor_counter :: Html
visitor_counter = [hsx|
    Visitors: <p id="visitor_count"></p>
    <script>
        async function visit(){
            await fetch("/api/visits/new", {

            })
            fetch("/api/visits/get", {}).then(response => {
                response.text().then(text => {
                    document.getElementById("visitor_count").innerHTML = text
                })
            })
        }
        visit()
    </script>

|]

index :: Html
index = [hsx|
    <h1>Skademaskinen</h1>
    {intro}
    {visitor_counter}
|]
