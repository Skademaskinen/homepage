module Helpers.Section where

import IHP.HSX.QQ
import Text.Blaze.Html

section :: Html -> Html
section content = [hsx|
    <div class="section">
        {content}
    </div>
    <hr>
|]