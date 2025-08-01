import java.io.File

data class Position2D(val x: Int, val y: Int)

fun usage() {
    System.err.println("usage: java -jar Program.jar <input file>")
    kotlin.system.exitProcess(1)
}

fun process(content: String) : Int {
    var santa = Position2D(0, 0)
    var roboSanta = Position2D(0, 0)
    var positions = mutableSetOf(santa)
    var santaMove = true
    var nextPosition: Position2D

    for ( ch in content ) {
        if ( santaMove ) {
            santa = when ( ch ) {
                '^' -> Position2D(santa.x, santa.y + 1)
                'v' -> Position2D(santa.x, santa.y - 1)
                '<' -> Position2D(santa.x - 1, santa.y)
                '>' -> Position2D(santa.x + 1, santa.y)
                else -> santa
            }
            nextPosition = santa
        } else {
            roboSanta = when ( ch ) {
                '^' -> Position2D(roboSanta.x, roboSanta.y + 1)
                'v' -> Position2D(roboSanta.x, roboSanta.y - 1)
                '<' -> Position2D(roboSanta.x - 1, roboSanta.y)
                '>' -> Position2D(roboSanta.x + 1, roboSanta.y)
                else -> roboSanta
            }
            nextPosition = roboSanta
        }
        positions.add(nextPosition)
        santaMove = !santaMove
    }

    return positions.count()
}

fun main(args: Array<String>) {
    if ( args.isEmpty()) {
        usage()
    }
    val filename = args[0]
    try {
        val content = File(filename).readText()
        val result = process(content)
        println("result = $result")
    } catch ( e: Exception ) {
        System.err.println("error reading file: ${e.message}")
    }
}
