import java.io.File

fun usage() {
    System.err.println("usage: java -jar Program.jar <input file>")
    kotlin.system.exitProcess(1)
}

fun process(content: String) : Int {
    var totalArea = 0

    for ( line in content.split("\n") ) {
        if ( !line.isEmpty() ) {
            val dims = line.split("x").map { it.toInt() }
            val l = dims[0]
            val w = dims[1]
            val h = dims[2]
            val area1 = l * w
            val area2 = l * h
            val area3 = w * h
            val surfaceArea = 2 * area1 + 2 * area2 + 2 * area3
            val minArea = listOf(area1, area2, area3).minOrNull() ?: 0
            totalArea += surfaceArea + minArea
        }
    }

    return totalArea
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
