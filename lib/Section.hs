module Section where

import IHP.HSX.QQ (hsx)
import Text.Blaze.Html (Html)

section :: Html -> Html
section content = [hsx|
    <div class="section">
        {content}
    </div>
    <hr>
|]