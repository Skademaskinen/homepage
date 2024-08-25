module Helpers.CodeBlock where

import IHP.HSX.QQ (hsx)
import Text.Blaze.Html (Html)

codeBlock :: String -> String -> Html
codeBlock language code = [hsx|
    <pre>
        <code class={"language-" ++ language}>
            {code}
        </code>
    </pre>
    <script src="/static/prism/prism.js"></script>
|]