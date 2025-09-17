import java.io.File

fun usage() {
    System.err.println("usage: java -jar Program.jar <input file>")
    kotlin.system.exitProcess(1)
}

fun process(content: String) : Int {
    var result = 0

    for ( line in content.split("\n") ) {
        val codeLen = line.length
        var memLen = 0
        var i = 1
        while ( i < line.length - 1 ) {
            when ( line.get(i) ) {
                '\\' -> {
                    when ( line.get(i + 1) ) {
                        '\\', '"' -> i += 2
                        'x' -> i += 4
                        else -> i += 1
                    }
                }
                else -> i += 1
            }
	    memLen += 1
        }
	result += codeLen - memLen
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
