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
        line.split(Regex(""":\s+""")).let { (_, rest) ->
            rest.split(Regex("""\s+\|\s+""")).let { (winningStr, handStr) ->
                var winningSet = HashSet<Int>()
                for ( numStr in winningStr.split(Regex("""\s+""")) ) {
                    val num = numStr.toIntOrNull()!!
                    winningSet.add(num);
                }
                var handSet = HashSet<Int>()
                for ( numStr in handStr.split(Regex("""\s+""")) ) {
                    val num = numStr.toIntOrNull()!!
                    handSet.add(num);
                }
                val intersection = winningSet.intersect(handSet)
                val count = intersection.size
                if ( count > 0 ) {
                    result += 1 shl (count - 1);
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
