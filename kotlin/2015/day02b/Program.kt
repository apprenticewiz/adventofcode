import java.io.File

fun usage() {
    System.err.println("usage: java -jar Program.jar <input file>")
    kotlin.system.exitProcess(1)
}

fun process(content: String) : Int {
    var totalLen = 0

    for ( line in content.split("\n") ) {
        if ( !line.isEmpty() ) {
            val dims = line.split("x").map { it.toInt() }
            val l = dims[0]
            val w = dims[1]
            val h = dims[2]
            val perim1 = 2 * (l + h)
            val perim2 = 2 * (l + w)
            val perim3 = 2 * (w + h)
            val presentLen = listOf(perim1, perim2, perim3).minOrNull() ?: 0
            val bowLen = l * w * h
            totalLen += presentLen + bowLen
        }
    }

    return totalLen
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
