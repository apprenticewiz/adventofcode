import java.io.File

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
                    var redNeeded = 0
                    var greenNeeded = 0
                    var blueNeeded = 0
                    for ( subsetStr in revealsStr.split("; ") ) {
                        for ( cubesStr in subsetStr.split(", ") ) {
                            cubesStr.split(' ').let { (amountStr, color) ->
                                val amount = amountStr.toIntOrNull()
                                if ( amount != null ) {
                                    when ( color ) {
                                        "red" -> if (amount > redNeeded) redNeeded = amount
                                        "green" -> if (amount > greenNeeded) greenNeeded = amount
                                        "blue" -> if (amount > blueNeeded) blueNeeded = amount
                                        else -> throw IllegalArgumentException("unknown color")
                                    }
                                }
                            }
                        }
                    }
                    result += redNeeded * greenNeeded * blueNeeded
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
