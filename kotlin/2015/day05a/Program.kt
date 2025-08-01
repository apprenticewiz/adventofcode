import java.io.File

fun usage() {
    System.err.println("usage: java -jar Program.jar <input file>")
    kotlin.system.exitProcess(1)
}

fun prop1(str: String) : Boolean {
    return Regex("[aeiou]").findAll(str).count() >= 3
}

fun prop2(str: String) : Boolean {
    return Regex("(.)\\1").containsMatchIn(str);
}

fun prop3(str: String) : Boolean {
    return !str.contains("ab") && !str.contains("cd") && !str.contains("pq") && !str.contains("xy")
}

fun process(content: String) : Int {
    var count = 0

    for ( line in content.split("\n") ) {
        if ( !line.isEmpty() ) {
            if ( prop1(line) && prop2(line) && prop3(line) ) {
                count += 1
            }
        }
    }

    return count
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
