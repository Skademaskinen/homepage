module Layout where

import IHP.HSX.QQ
import Text.Blaze.Html

import Header
import Footer
import Stylesheet

layout :: Html -> Html
layout content = [hsx|
    {stylesheet}
    <html>
        {header [
            ("Old Site", "https://about.skademaskinen.win")
        ]}
        <body>
            {content}
        </body>
        {footer}
    </html>
|]
