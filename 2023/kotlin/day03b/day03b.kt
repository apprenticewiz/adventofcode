import java.io.File

data class Position (val row: Int, val col: Int)

fun usage() {
    println("usage: java -jar <jarfile> <infile>")
    System.exit(1)
}

fun buildNumbers(contents: String): HashMap<Position, String> {
    var numberLocs = HashMap<Position, String>()
    var scanningNumber = false
    var number = StringBuilder()
    var currentPos = Position(row = -1, col = -1)
    var row = 0
    for ( line in contents.lines() ) {
        if ( line.isEmpty() ) {
            break
        }
        for ( col in 0..(line.length - 1) ) {
            var ch = line[col]
            if ( scanningNumber ) {
                if ( ch.isDigit() ) {
                    number.append(ch)
                } else {
                    numberLocs[currentPos] = number.toString()
                    scanningNumber = false
                    number = StringBuilder()
                }
            } else {
                if ( ch.isDigit() ) {
                    number.append(ch)
                    currentPos = Position(row = row, col = col)
                    scanningNumber = true
                }
            }
        }
        if ( scanningNumber ) {
            numberLocs[currentPos] = number.toString()
            scanningNumber = false
            number = StringBuilder()
        }
        row++
    }
    return numberLocs
}

fun buildParts(contents: String): HashMap<Position, Char> {
    var partLocs = HashMap<Position, Char>()
    var row = 0
    for ( line in contents.lines() ) {
        if ( line.isEmpty() ) {
            break
        }
        for ( col in 0..(line.length - 1) ) {
            val ch = line[col]
            if ( ch == '*' ) {
                partLocs[Position(row = row, col = col)] = ch;
            }
        }
        row++
    }
    return partLocs
}

fun checkParts(numberLocs: HashMap<Position, String>, partLocs: HashMap<Position, Char>): Int {
    var result = 0
    val neighbors = listOf(
        Position(row = -1, col = -1), Position(row = -1, col = 0), Position(row = -1, col = 1),
        Position(row = 0, col = -1), Position(row = 0, col = 1),
        Position(row = 1, col = -1), Position(row = 1, col = 0), Position(row = 1, col = 1),
    )
    for ( partLoc in partLocs.keys ) {
        var adjacentCount = 0
        var prod = 1
        for ( numberLocEntry in numberLocs.entries ) {
            val numberLoc = numberLocEntry.key
            val number = numberLocEntry.value
            val numberRow = numberLoc.row
            val numberColFirst = numberLoc.col
            val numberColLast = numberLoc.col + number.length - 1
            var found = false
            for ( neighbor in neighbors ) {
                val adjacent = Position(row = (partLoc.row + neighbor.row), col = (partLoc.col + neighbor.col))
                for ( numberCol in numberColFirst..numberColLast ) {
                    if ( numberRow == adjacent.row && numberCol == adjacent.col ) {
                        adjacentCount++
                        prod *= number.toInt()
                        found = true
                        break
                    }
                }
                if ( found ) {
                    break
                }
            }
        }
        if ( adjacentCount == 2 ) {
            result += prod
        }
    }
    return result
}

fun process(contents: String): Int {
    var numberLocs = buildNumbers(contents)
    var partLocs = buildParts(contents)
    return checkParts(numberLocs, partLocs)
}

fun main(args: Array<String>) {
    if ( args.size < 1 ) {
        usage()
    }
    val filename = args[0]
    val contents = File(filename).readText()
    val result = process(contents)
    println("result = $result")
}
