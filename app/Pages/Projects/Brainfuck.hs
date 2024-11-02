module Pages.Projects.Brainfuck where

import Data.List (intercalate)
import IHP.HSX.QQ (hsx)
import Text.Blaze.Html (Html)

import CodeBlock (codeBlock)

convertSymbols :: String -> String
convertSymbols ('+':xs) = "(*ptr)++;\n" ++ convertSymbols xs
convertSymbols ('-':xs) = "(*ptr)--;\n" ++ convertSymbols xs
convertSymbols ('>':xs) = "ptr++;\n" ++ convertSymbols xs
convertSymbols ('<':xs) = "ptr--;\n" ++ convertSymbols xs
convertSymbols ('[':xs) = "while(*ptr) {\n" ++ convertSymbols xs
convertSymbols (']':xs) = "}\n" ++ convertSymbols xs
convertSymbols ('.':xs) = "printf(\"%c\", (*ptr));\n" ++ convertSymbols xs
convertSymbols (',':xs) = "scanf(\"%c\", ptr);\n" ++ convertSymbols xs
convertSymbols (x : xs) = convertSymbols xs
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
    <br>
    Use the script below to compile and run a file containing brainfuck code.
    {codeBlock "bash" $ intercalate "\n" [
        "#!/usr/bin/env bash",
        "file=$1",
        "if [ -z $TMPDIR ]; then",
        "\tout_file=\"/tmp/code_bf\"",
        "else",
        "\tout_file=\"$TMPDIR/code_bf\"",
        "fi",
        "b_source=\"$(cat $file)\"",
        "c_source=\"$(curl -d \"$b_source\" -X \"POST\" https://skade.dev/api/brainfuck 2> /dev/null)\"",
        "echo \"$c_source\" | gcc -xc - -o $out_file",
        "$out_file"
    ]}
    I think a really good example of brainfuck programs is: <a href="http://www.brainfuck.org/life.b">http://www.brainfuck.org/life.b</a>
    <br>
    Run it directly:
    {codeBlock "bash" "curl https://skade.dev/api/brainfuck -d \"$(curl https://brainfuck.org/life.b 2> /dev/null)\" 2> /dev/null | gcc -xc - -o /tmp/life && /tmp/life"}
|]
