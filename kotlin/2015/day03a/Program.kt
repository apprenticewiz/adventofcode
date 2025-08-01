import java.io.File

data class Position2D(val x: Int, val y: Int)

fun usage() {
    System.err.println("usage: java -jar Program.jar <input file>")
    kotlin.system.exitProcess(1)
}

fun process(content: String) : Int {
    var santa = Position2D(0, 0)
    var positions = mutableSetOf(santa)

    for ( ch in content ) {
        santa = when ( ch ) {
            '^' -> Position2D(santa.x, santa.y + 1)
            'v' -> Position2D(santa.x, santa.y - 1)
            '<' -> Position2D(santa.x - 1, santa.y)
            '>' -> Position2D(santa.x + 1, santa.y)
            else -> santa
        }
        positions.add(santa)
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
