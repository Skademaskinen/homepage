console.log("Initializing snake game")

function newFruit() {
    var coords = [Math.floor(Math.random()*20), Math.floor(Math.random()*20)]
    if (tiles[coords[1]][coords[0]].style.backgroundColor != "black")
        return newFruit()
    
    fruits.push(coords)
    tiles[coords[1]][coords[0]].style.backgroundColor = "yellow"
    return coords
}

async function launchGame() {
    document.getElementById("snakefire").style.top = "100%"
    var running = true
    try {
        while (running) {
            tail.push([x, y])
            switch (direction) {
                case "W":
                    y--
                    break
                case "A":
                    x--
                    break
                case "S":
                    y++
                    break
                case "D":
                    x++
                    break
                default:
                    running = false
            }
            direction = next_direction
            if(tiles[y][x].style.backgroundColor == "red")
                running = false

            tiles[y][x].style.backgroundColor = "red"
            var new_fruits = []
            var score_increase = false
            fruits.forEach(([fruit_x, fruit_y]) => {
                if (x == fruit_x && y == fruit_y) {
                    console.log("Score increase!")
                    score_increase = true
                    score++
                    new_fruits.push(newFruit())
                    if (score < 50) {
                        document.getElementById("snakefire").style.top = (100-score) + "%"
                    }
                } else {
                    new_fruits.push([fruit_x, fruit_y])
                }
            })
            fruits = new_fruits
            if(!score_increase){
                var coords = tail.shift()
                tiles[coords[1]][coords[0]].style.backgroundColor = "black"
            }
            document.getElementById("snake-score").innerHTML = score
            await sleep(100-speed)
        }
    }
    finally{
        alert("Game over")
        started = false
        x = 5
        y = 5
        tail = []
        fruits = []
        var scoreboard = document.getElementById("scoreboard")
        var date = new Date()
        var tr = document.createElement("tr")
        var time = ("" + date.getHours()) + ":" + date.getMinutes() + ":" + date.getSeconds()
        var timetd = document.createElement("td")
        timetd.innerHTML = time
        var scoretd = document.createElement("td")
        scoretd.innerHTML = score
        var speedtd = document.createElement("td")
        speedtd.innerHTML = speed
        var fruitstd = document.createElement("td")
        fruitstd.innerHTML = fruit_count
        tr.appendChild(timetd)
        tr.appendChild(scoretd)
        tr.appendChild(speedtd)
        tr.appendChild(fruitstd)
        scoreboard.appendChild(tr)
        
        var name = document.getElementById("player-name").value

        if(name != "") {
            fetch("/api/snake/add", {
                method: "PUT",
                body: JSON.stringify({
                    timestamp: Math.floor(Date.now() / 1000),
                    name: document.getElementById("player-name").value,
                    score: score,
                    speed: speed,
                    fruits: fruit_count
                })
            })
        }
    }
}

var tiles = []
var fruits = []
var tail = []
var fruit_count = 1
var speed = 25
var score = 0
var direction = "N"
var next_direction = "N"
var x = 5
var y = 5

var row = []
for (var index = 0; index <= 20*20; index++) {
    if (row.length == 20) {
        tiles.push(row)
        row = []
    }
    row.push(document.getElementById("tile-"+index))
}
var started = false

function keyEvent(id){
    switch (id) {
        case 13:
            if (!started) {
                tiles.forEach(row => row.forEach(tile => tile.style.backgroundColor = "black"))
                var speed_input = document.getElementById("speed")
                var fruits_input = document.getElementById("fruits")
                if(speed_input.value != "")
                    speed = parseFloat(speed_input.value)
                
                if(fruits_input.value != "")
                    fruit_count = parseInt(fruits_input.value)
                started = !started
                score = 0
                for (var i = 0; i < fruit_count; i++)
                    newFruit()

                direction = "D"
                next_direction = "D"
                launchGame()
            }
            break
        case 87:
        case 38:
            if (direction != "S")
                next_direction = "W"
                break
        case 65:
        case 37:
            if(direction != "D")
                next_direction = "A"
            break
        case 83:
        case 40:
            if(direction != "W")
                next_direction = "S"
            break
        case 68:
        case 39:
            if(direction != "A")
                next_direction = "D"
            break
    }
}

document.addEventListener("keydown", event => keyEvent(event.keyCode)) 
