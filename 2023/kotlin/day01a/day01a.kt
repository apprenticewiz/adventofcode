import java.io.File

fun usage() {
    println("usage: java -jar <jarfile> <infile>")
    System.exit(1)
}

fun process(contents: String): Int {
    var sum = 0
    val digits = listOf("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")
    for ( line in contents.lines() ) {
        var minIndex: Int? = null
        var maxIndex: Int? = null
        var leftDigit: Char? = null
        var rightDigit: Char? = null
        if ( line.isEmpty() ) {
            break
        }
        digits.forEach { digit ->
            if ( line.contains(digit) ) {
                line.indexOf(digit).let { leftIndex ->
                    if (minIndex == null || leftIndex < minIndex!!) {
                        minIndex = leftIndex
                        leftDigit = digit.first()
                    }
                }
                line.lastIndexOf(digit).let { rightIndex ->
                    if (maxIndex == null || rightIndex > maxIndex!!) {
                        maxIndex = rightIndex
                        rightDigit = digit.first()
                    }
                }
            }
        }
        sum += (((leftDigit!!.code) - ('0'.code)) * 10) + ((rightDigit!!.code) - ('0'.code))
    }
    return sum
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
