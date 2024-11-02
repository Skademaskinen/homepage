module CodeBlock where

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

hsxIntroCodeBlock :: Html
hsxIntroCodeBlock = codeBlock "haskell" $ mconcat [
    "view :: Html\n", 
    "view = ", rB, "hsx", vl, "\n", 
    "\t<span>Hello, </span>\n",
    vl, lB, "\n\n", 
    "index :: Html\n",
    "index = ", rB, "hsx", vl, "\n",
    "\t{view}\n",
    "\t<span>World!</span>\n",
    vl, lB
    ]

introCodeView :: Html
introCodeView = [hsx|
    <span>Hello, </span>
|]
introCodeIndex :: Html
introCodeIndex = [hsx|
    {introCodeView}
    <span>World!</span>
|]

rB :: String
rB = "["
lB :: String
lB = "]"
vl :: String
vl = "|"

wrapHsxCode :: Html -> Html
wrapHsxCode code = [hsx|
    {rB}hsx{vl}
        {code}
    {vl}{lB}
|]
