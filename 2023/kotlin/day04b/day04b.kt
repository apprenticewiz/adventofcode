import java.io.File

fun usage() {
    println("usage: java -jar <jarfile> <infile>")
    System.exit(1)
}

fun process(contents: String): Int {
    var result = 0
    var instances = HashMap<Int, Int>()
    for ( line in contents.lines() ) {
        if ( line.isEmpty() ) {
            break
        }
        line.split(Regex(""":\s+""")).let { (cardPart, rest) ->
            cardPart.split(Regex("""\s+""")).let { (_, cardNumStr) ->
                val cardNum = cardNumStr.toIntOrNull()!!
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
                    for ( i in (cardNum + 1)..(cardNum + count) ) {
                        val copies = (instances.get(i) ?: 0) + 1 + (instances.get(cardNum) ?: 0)
                        instances.put(i, copies)
                    }
                }
            }
            result += 1
        }
    }
    for ( value in instances.values ) {
        result += value
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
