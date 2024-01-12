import java.io.File

fun usage() {
    println("usage: java -jar <jarfile> <infile>")
    System.exit(1)
}

fun process(contents: String): Int {
    var sum = 0
    val digitsMap = hashMapOf(
        "0" to 0,
        "1" to 1,
        "2" to 2,
        "3" to 3,
        "4" to 4,
        "5" to 5,
        "6" to 6,
        "7" to 7,
        "8" to 8,
        "9" to 9,
        "zero" to 0,
        "one" to 1,
        "two" to 2,
        "three" to 3,
        "four" to 4,
        "five" to 5,
        "six" to 6,
        "seven" to 7,
        "eight" to 8,
        "nine" to 9
    )
    for ( line in contents.lines() ) {
        var minIndex: Int? = null
        var maxIndex: Int? = null
        var leftDigit: Int? = null
        var rightDigit: Int? = null
        if ( line.isEmpty() ) {
            break
        }
        digitsMap.keys.forEach { digit ->
            if ( line.contains(digit) ) {
                line.indexOf(digit).let { leftIndex ->
                    if (minIndex == null || leftIndex < minIndex!!) {
                        minIndex = leftIndex
                        leftDigit = digitsMap[digit]
                    }
                }
                line.lastIndexOf(digit).let { rightIndex ->
                    if (maxIndex == null || rightIndex > maxIndex!!) {
                        maxIndex = rightIndex
                        rightDigit = digitsMap[digit]
                    }
                }
            }
        }
        sum += leftDigit!! * 10 + rightDigit!!
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
