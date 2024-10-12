module Pages.Contact.Contact where

import IHP.HSX.QQ (hsx)
import Text.Blaze.Html (Html)

import Utils ( linkImage, row )
import Page (Page, PageSetting (Description, Route, EmbedText, EmbedImage))
import Layout (layout)

rows :: [Html]
rows = [
    row [[hsx|Type|],              [hsx|Address|]],
    row [[hsx|Email|],             [hsx|<a href="mailto://mast3r@skade.dev">mast3r@skade.dev</a>|]],
    row [[hsx|University Email|],  [hsx|<a href="mailto://tjen19@student.aau.dk">tjen19@student.aau.dk</a>|]],
    row [[hsx|Matrix|],            [hsx|@mast3r:skade.dev|]],
    row [[hsx|Discord|],           [hsx|mast3r_waf1z|]]
    ]

page :: Html
page = [hsx|
    <h1>Contact me</h1>
        Here's my various contact addresses
        <br>
        <table style="display:inline-block;border:1px solid white; padding:5px;">
            {mconcat rows}
        </table>

    <h2>Socials</h2>
    Links to my socials
    <br>
    {linkImage "LinkedIn" "/static/contact/LinkedIn.png" "https://www.linkedin.com/in/thomas-m%C3%B8ller-j-a76601a6/"}

    <h2>University</h2>

    <div style="border:1px solid white; border-radius: 10px; margin: 0% 30% 0% 30%">
        <img src="/static/contact/fspinner.gif" style="width:150px; height:150px;">
        <p>Where to find me: <br><a href="https://fklub.dk/jaegerstuen">Jægerstuen</a> - Selma Lagerløfs Vej 300, 9220 Aalborg Øst</p><br>
    </div><br>
|]

settings :: [PageSetting]
settings = [
    Route "/contact", 
    EmbedText "Skademaskinen - Contact me",
    EmbedImage "/static/icon.png",
    Description "My contact information, and where to find me"
    ]

contact :: Page
contact = (settings, const $ return $ layout page)
