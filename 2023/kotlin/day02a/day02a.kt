import java.io.File

const val TOTAL_RED = 12
const val TOTAL_GREEN = 13
const val TOTAL_BLUE = 14

fun usage() {
    println("usage: java -jar <jarfile> <infile>")
    System.exit(1)
}

fun process(contents: String): Int {
    var result = 0
    for ( line in contents.lines() ) {
        if ( line.isEmpty() ) {
            break
        }
        line.split(": ").let { (gameStr, revealsStr) ->
            gameStr.split(' ').let { (_, gameNumStr) ->
                val gameNum = gameNumStr.toIntOrNull()
                if ( gameNum != null ) {
                    var valid = true
                    for ( subsetStr in revealsStr.split("; ") ) {
                        for ( cubesStr in subsetStr.split(", ") ) {
                            cubesStr.split(' ').let { (amountStr, color) ->
                                val amount = amountStr.toIntOrNull()
                                if ( amount != null ) {
                                    when (color) {
                                        "red" -> if (amount > TOTAL_RED) valid = false
                                        "green" -> if (amount > TOTAL_GREEN) valid = false
                                        "blue" -> if (amount > TOTAL_BLUE) valid = false
                                        else -> throw IllegalArgumentException("unknown color")
                                    }
                                }
                            }
                        }
                    }
                    if ( valid ) {
                        result += gameNum
                    }
                }
            }
        }
    }
    return result
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
