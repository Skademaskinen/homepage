module Pages.Projects.Editor where

import Text.Blaze.Html (Html)
import IHP.HSX.QQ (hsx)

styles :: Html
styles = [hsx|
    <style>
        .sidebar * {
            all: unset;
            width: 200px;
            border 0px solid black;
            border-radius: 0px;
            padding: 2px;
            margin: 0px;
        }
        .sidebar button {
            background-color: #222222;
            color: white;
        }
        .sidebar button:hover {
            background-color: #444444;
        }
        .sidebar_input {
            all: unset;
            width: 200px;
            border: 0px solid black;
            padding: 2px;
            margin: 0px;
            background-color: white;
            color: black;
        }
        .editor {
            all: unset;
            width: 100%;
            height: 400px;
            background-color: #151515;
            color: white;
            white-space: pre-wrap;
            resize: none;
            text-align: left;
            padding: 5px;
        }
        
    </style>
|]

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
                var items = document.getElementById("sidebar").getElementsByTagName("button")
                for(i = 0; i < items.length; i++) {
                    if (items[i].innerHTML == filename) {
                        items[i].style.backgroundColor = "#444444"
                    }
                    else {
                        items[i].style.backgroundColor = "#222222"
                    }
                }
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
    <input class="sidebar_input" id="new-file" placeholder="Write name of new file">
    <div class="sidebar" id="sidebar"></div>
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
    <textarea class="editor" id="editor">
    </textarea><br>
    <button onclick="save_file()">Save File</button>
    <button onclick="delete_file()">Delete File</button>
|]

editor :: Html
editor = [hsx|
    {styles}
    For this project, i've written a text editor, feel free to make the files you want, i was considering putting a field for specifying a method to run it but i don't want to create obvious vulnerabilities..
    {commands}

    <table>
        <tr>
            <td style="width: 300px; text-align: left; vertical-align: top;">
                {sidebar}
            </td>
            <td style="width: 100%; vertical-align: top;">
                {mainFrame}
            </td>
        </tr>
    </table>
    <span id="filename"></span> | <span id="status"></span>
|]
