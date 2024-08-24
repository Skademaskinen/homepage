module Layout where

import IHP.HSX.QQ
import Text.Blaze.Html

import Header
import Footer

layout :: Html -> Html
layout content = [hsx|
    <link rel="stylesheet" href="/static/stylesheet.css">
    <link rel="stylesheet" href="/static/prism/prism.css">
    <html>
        {header [
            ("Home", "/"),
            ("Contact", "/contact"),
            ("Projects", "/projects"),
            ("Old Site", "https://about.skademaskinen.win")
        ]}
        <body>
            <div class="mainmatter">
                <hr>
                {content}
            </div>
        </body>
        {footer}
    </html>
|]
