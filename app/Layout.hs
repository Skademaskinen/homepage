module Layout where

import IHP.HSX.QQ (hsx)
import Text.Blaze.Html (Html)

import Footer (footer)
import Header (header)
import Network.Wai (Request)

layout :: Html -> Html
layout content = [hsx|
    <!DOCTYPE html>
    <html>
        <head>
            <link rel="stylesheet" href="/static/stylesheet.css">
            <link rel="stylesheet" href="/static/prism/prism.css">
            <meta charset="UTF-8">
        </head>
        <script src="/static/app.js"></script>
        {header [
            ("Home", "/"),
            ("Contact", "/contact"),
            ("Projects", "/projects"),
            ("Sources", "/sources"),
            ("Guestbook", "/guestbook"),
            ("Old Site", "https://about.skademaskinen.win")
        ]}
        <title>Skademaskinen</title>
        <body>
            <div class="mainmatter">
                <hr>
                {content}
                <hr>
            </div>
        </body>
    </html>
|]
