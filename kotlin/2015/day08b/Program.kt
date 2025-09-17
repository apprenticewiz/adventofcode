import java.io.File

fun usage() {
    System.err.println("usage: java -jar Program.jar <input file>")
    kotlin.system.exitProcess(1)
}

fun process(content: String) : Int {
    var result = 0

    for ( line in content.split("\n") ) {
        if ( line.length > 0 ) {
            val codeLen = line.length
            var encLen = 0
            for ( i in 0 until line.length ) {
                when ( line.get(i) ) {
                    '\\', '"' -> encLen += 2
                    else -> encLen += 1
                }
            }
            result += 2 + (encLen - codeLen)
        }
    }
    return result
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
