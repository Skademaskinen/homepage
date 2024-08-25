module Pages.Contact.Contact where

import IHP.HSX.QQ
import Text.Blaze.Html

import Helpers.Utils

link_image :: String -> String -> String -> Html 
link_image label image url = [hsx|
    <div class="inline_container">
        <a href={url}>
            <img src={image} width="100" height="100" style="padding: 1px;">
        </a>
        <br>
        {label}
    </div>
|]

items :: [String] -> Html
items values = forEach [1..length values] (\i -> [hsx|
    <th style="width:100px">{values !! (i-1)}</th>
|])

row :: [String] -> Html
row values = [hsx|
    <tr>
        {items values}
    </tr>
|]

contact :: Html
contact = [hsx|
    <h1>Contact me</h1>
        Here's my various contact addresses
        <br>
        <table style="display:inline-block;border:1px solid white; padding:5px;">
            {row ["Type", "Address"]}
            {row ["Email", "mast3r@skade.dev"]}
            {row ["University Email", "tjen19@student.aau.dk"]}
            {row ["Matrix", "mast3r@skade.dev"]}
            {row ["Discord", "mast3r_waf1z"]}
        </table>

    <h2>Socials</h2>
    Links to my socials
    <br>
    {link_image "LinkedIn" "/static/contact/LinkedIn.png" "https://www.linkedin.com/in/thomas-m%C3%B8ller-j-a76601a6/"}
|]