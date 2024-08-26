module Pages.Contact.Contact where

import IHP.HSX.QQ (hsx)
import Text.Blaze.Html (Html)

import Helpers.Utils ( linkImage, row )

contact :: Html
contact = [hsx|
    <h1>Contact me</h1>
        Here's my various contact addresses
        <br>
        <table style="display:inline-block;border:1px solid white; padding:5px;">
            {row ["Type", "Address"]}
            {row ["Email", "mast3r@skade.dev"]}
            {row ["University Email", "tjen19@student.aau.dk"]}
            {row ["Matrix", "@mast3r:skade.dev"]}
            {row ["Discord", "mast3r_waf1z"]}
        </table>

    <h2>Socials</h2>
    Links to my socials
    <br>
    {linkImage "LinkedIn" "/static/contact/LinkedIn.png" "https://www.linkedin.com/in/thomas-m%C3%B8ller-j-a76601a6/"}
|]
