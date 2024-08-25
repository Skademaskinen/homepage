module Layout where

import IHP.HSX.QQ
import Text.Blaze.Html

import Header
import Footer

layout :: Html -> Html
layout content = [hsx|
    <!DOCTYPE html>
    <link rel="stylesheet" href="/static/stylesheet.css">
    <link rel="stylesheet" href="/static/prism/prism.css">
    <html>
        <script src="/static/app.js"></script>
        {header [
            ("Home", "/"),
            ("Contact", "/contact"),
            ("Projects", "/projects"),
            ("Sources", "/sources"),
            ("Old Site", "https://about.skademaskinen.win")
        ]}
        <body>
            <div class="mainmatter">
                <hr>
                {content}
                <hr>
            </div>
        </body>
        {footer}
    </html>
|]
