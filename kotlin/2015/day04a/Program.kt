import java.security.MessageDigest

fun usage() {
    System.err.println("usage: java -jar Program.jar <input file>")
    kotlin.system.exitProcess(1)
}

fun process(key: String) : Int {
    var n = 1

    while ( true ) {
        val tryKey = "$key$n"
        val digest = MessageDigest.getInstance("MD5").digest(tryKey.toByteArray())
        val hexBytes = digest.joinToString("") { "%02x".format(it) }
        if ( hexBytes.startsWith("00000") ) {
            return n
        } else {
            n += 1
        }
    }
}

fun main(args: Array<String>) {
    if ( args.isEmpty()) {
        usage()
    }
    val key = args[0]
    val result = process(key)
    println("result = $result")
}
