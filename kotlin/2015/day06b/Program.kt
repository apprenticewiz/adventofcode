import java.io.File
import java.util.regex.Pattern
import java.util.regex.Matcher

data class Position2D<T>(val x: T, val y: T)

class Bounds(val upperLeft: Position2D<Int>, val lowerRight: Position2D<Int>)

class Grid {
    companion object {
        const val ROW_MAX = 1000
        const val COL_MAX = 1000
    }

    private val grid = Array(ROW_MAX) { IntArray(COL_MAX) { 0 } }

    fun perform(action: String, bounds: Bounds) {
        for ( row in bounds.upperLeft.x..bounds.lowerRight.x ) {
            for ( col in bounds.upperLeft.y..bounds.lowerRight.y ) {
                when ( action ) {
                    "turn on" -> grid[row][col] += 1
                    "turn off" -> grid[row][col] = maxOf(0, grid[row][col] - 1)
                    "toggle" -> grid[row][col] += 2
                }
            }
        }
    }

    fun count(): Int {
        var total = 0
        for ( row in 0 until ROW_MAX ) {
            for ( col in 0 until COL_MAX ) {
                total += grid[row][col]
            }
        }
        return total
    }
}

fun usage() {
    System.err.println("usage: java -jar Program.jar <input file>")
    kotlin.system.exitProcess(1)
}

fun process(content: String) : Int {
    var grid = Grid()
    var pattern = Pattern.compile("(turn on|turn off|toggle) (\\d+),(\\d+) through (\\d+),(\\d+)")

    for ( line in content.split("\n") ) {
        if ( !line.isEmpty() ) {
            val matcher = pattern.matcher(line)
            if ( matcher.matches() ) {
                val action = matcher.group(1)
                val r1 = matcher.group(2).toInt()
                val c1 = matcher.group(3).toInt()
                val upperLeft = Position2D(r1, c1)
                val r2 = matcher.group(4).toInt()
                val c2 = matcher.group(5).toInt()
                val lowerRight = Position2D(r2, c2)
                val bounds = Bounds(upperLeft, lowerRight)
                grid.perform(action, bounds)
            }
        }
    }

    return grid.count()
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
        System.err.println("error processing file: ${e.message}")
    }
}
