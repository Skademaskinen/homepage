module Layout where

import IHP.HSX.QQ (hsx)
import Text.Blaze.Html (Html)

import Header ( header )
import Footer ( footer )

layout :: Html -> Html
layout content = [hsx|
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
        <title>Skademaskinen</title>
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
