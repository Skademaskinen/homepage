module Pages.Projects.Brainfuck where

import IHP.HSX.QQ (hsx)
import Text.Blaze.Html (Html)
import Data.List (intercalate)

import CodeBlock (codeBlock)

convertSymbols :: String -> String
convertSymbols ('+':xs) = "(*ptr)++;\n"++convertSymbols xs
convertSymbols ('-':xs) = "(*ptr)--;\n"++convertSymbols xs
convertSymbols ('>':xs) = "ptr++;\n"++convertSymbols xs
convertSymbols ('<':xs) = "ptr--;\n"++convertSymbols xs
convertSymbols ('[':xs) = "while(*ptr) {\n"++convertSymbols xs
convertSymbols (']':xs) = "}\n"++convertSymbols xs
convertSymbols ('.':xs) = "printf(\"%c\", (*ptr));\n"++convertSymbols xs
convertSymbols (',':xs) = "scanf(\"%c\", ptr);\n"++convertSymbols xs
convertSymbols (x:xs) = convertSymbols xs
convertSymbols [] = []

code :: String -> String
code input = intercalate "\n" [
    "#include <stdio.h>",
    "#include <stdint.h>",
    "#include <inttypes.h>",
    "char buffer[30000] = {0};",
    "char* ptr = buffer;",
    "int main () {",
    convertSymbols input,
    "}"
    ]

brainfuck :: Html
brainfuck = [hsx|
    Write some brainfuck code, and you will receive the equivalent in C code.<br>
    <script>
        function download(file, text) {
            var element = document.createElement("a")
            element.setAttribute('href', 'data:text/plain;charset=utf-8, ' + encodeURIComponent(text))
            element.setAttribute('download', file)
            document.body.appendChild(element)
            element.click()
            document.body.removeChild(element)
        }
        function submit() {
            var source = document.getElementById("code").value
            fetch("/api/brainfuck", {
                method: "post",
                body: source
            }).then(response => response.text().then(text => {
                download("brainfuck.c", text)
            }))

        }
    </script>
    <textarea type="text" id="code" style="min-width: 600px; min-height: 200px;"></textarea>
    <br>
    <button id="submit" onclick="submit()">Submit</button>
    <br>
    Example:
    {codeBlock "txt" "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."}
|]