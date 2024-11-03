module Pages.Projects.Editor where

import Text.Blaze.Html (Html)
import IHP.HSX.QQ (hsx)

commands :: Html
commands = [hsx|
    <script>
        function status_message(message) {
            var status = document.getElementById("status")
            status.innerHTML = message
        }

        function switch_file(filename) {
            console.log("Switched pane")
            fetch("/api/editor/content/" + filename, {
                method: "GET"
            }).then(response => response.text().then(text => {
                document.getElementById("editor").value = text
                document.getElementById("filename").innerHTML = filename
                status_message("switched file")
            }))
        }

        function populate_sidebar() {
            fetch("/api/editor/sidebar", {
                method: "GET"
            }).then(response => response.json().then(async json => {
                var sidebar = document.getElementById("sidebar")
                await json.forEach(item => {
                    if([".", ".."].includes(item))
                        return
                    var b = document.createElement("button")
                    console.log(item)
                    b.innerHTML = item
                    b.onclick = _ => switch_file(item)
                    sidebar.append(b)
                    sidebar.appendChild(document.createElement("br"))
                })
                sidebar.children[0].onclick()

            }))
        }

        function save_file() {
            var filename = document.getElementById("filename").innerHTML
            var content = document.getElementById("editor").value
            fetch("/api/editor/content/" + filename, {
                method: "PUT",
                body: content
            }).then(_ => status_message("Successfully saved file"))
        }
        function delete_file() {
            var filename = document.getElementById("filename").innerHTML
            fetch("/api/editor/delete", {
                method: "DELETE",
                body: filename
            }).then(async _ => {
                sidebar.innerHTML = ""
                await populate_sidebar()
                status_message("deleted file!")
            })
        }
    </script>
|]


sidebar :: Html
sidebar = [hsx|
    <input id="new-file">
    <div id="sidebar"></div>
    <script>
        var new_file = document.getElementById("new-file")
        new_file.onkeypress = event => {
            if (event.keyCode == 13) {
                fetch("/api/editor/new", {
                    method: "POST",
                    body: new_file.value
                }).then(async response => {
                    if (response.status == 200) {
                        sidebar.innerHTML = ""
                        await populate_sidebar()
                        switch_file(new_file.value)
                    }
                    else {
                        status_message("Error, file already exists")
                    }
                })
            }
        }
        populate_sidebar()
    </script>
|]

mainFrame :: Html
mainFrame = [hsx|
    <h3 id="filename"></h3>
    <textarea style="min-width: 700px; min-height: 300px;" id="editor">
    </textarea><br>
    <button onclick="save_file()">Save File</button>
    <button onclick="delete_file()">Delete File</button>
|]

editor :: Html
editor = [hsx|
    For this project, i've written a text editor, feel free to make the files you want, i was considering putting a field for specifying a method to run it but i don't want to create obvious vulnerabilities..
    {commands}
    <table>
        <tr>
            <th style="width: 300px; text-align: left;">
                {sidebar}
            </th>
            <th>
                {mainFrame}
            </th>
        </tr>
    </table>
    <p id="status"></p>
|]
