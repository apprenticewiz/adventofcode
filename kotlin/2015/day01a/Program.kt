import java.io.File

fun usage() {
    System.err.println("usage: java -jar Program.jar <input file>")
    kotlin.system.exitProcess(1)
}

fun process(content: String) : Int {
    var floors = 0

    for ( ch in content ) {
        when ( ch ) {
            '(' -> floors += 1
            ')' -> floors -= 1
            else -> {}
        }
    }

    return floors
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
