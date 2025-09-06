import java.io.File
import java.util.regex.Pattern
import java.util.regex.Matcher

sealed class Operation {
    data class Assign(val src: String) : Operation()
    data class Not(val src: String) : Operation()
    data class And(val src1: String, val src2: String) : Operation()
    data class Or(val src1: String, val src2: String) : Operation()
    data class LeftShift(val src: String, val amt: Int) : Operation()
    data class RightShift(val src: String, val amt: Int) : Operation()
}

fun usage() {
    System.err.println("usage: java -jar Program.jar <input file>")
    kotlin.system.exitProcess(1)
}

fun process(content: String) : Int {
    val operations = HashMap<String, Operation>()
    val p1 = Pattern.compile("^(\\d+|\\w+) -> (\\w+)$")
    val p2 = Pattern.compile("NOT (\\d+|\\w+) -> (\\w+)")
    val p3 = Pattern.compile("(\\d+|\\w+) (AND|OR) (\\d+|\\w+) -> (\\w+)")
    val p4 = Pattern.compile("(\\d+|\\w+) (LSHIFT|RSHIFT) (\\d+) -> (\\w+)")

    for ( line in content.split("\n") ) {
        if ( !line.isBlank() ) {
            var matcher = p1.matcher(line)
            if ( matcher.matches() ) {
                val src = matcher.group(1)
                val dest = matcher.group(2)
                operations[dest] = Operation.Assign(src)
            }
            matcher = p2.matcher(line)
            if ( matcher.matches() ) {
                val src = matcher.group(1)
                val dest = matcher.group(2)
                operations[dest] = Operation.Not(src)
            }
            matcher = p3.matcher(line)
            if ( matcher.matches() ) {
                val src1 = matcher.group(1)
                val op = matcher.group(2)
                val src2 = matcher.group(3)
                val dest = matcher.group(4)
                if ( op == "AND" ) {
                    operations[dest] = Operation.And(src1, src2)
                } else {
                    operations[dest] = Operation.Or(src1, src2)
                }
            }
            matcher = p4.matcher(line)
            if ( matcher.matches() ) {
                val src = matcher.group(1)
                val op = matcher.group(2)
                val amt = matcher.group(3).toInt()
                val dest = matcher.group(4)
                if ( op == "LSHIFT" ) {
                    operations[dest] = Operation.LeftShift(src, amt)
                } else {
                    operations[dest] = Operation.RightShift(src, amt)
                }
            }
        }
    }

    val a = eval(operations, HashMap<String, Int>(), "a")
    operations["b"] = Operation.Assign(a.toString())
    return eval(operations, HashMap<String, Int>(), "a")
}

fun eval(ops: HashMap<String, Operation>, cache: HashMap<String, Int>, expr: String) : Int {
    if ( expr.toIntOrNull() != null ) {
        return expr.toInt()
    } else if ( cache.containsKey(expr) ) {
        return cache[expr]!!
    } else {
        val op = ops[expr]!!
        val r = when ( op ) {
            is Operation.Assign -> eval(ops, cache, op.src)
            is Operation.Not -> eval(ops, cache, op.src).inv()
            is Operation.And -> eval(ops, cache, op.src1) and eval(ops, cache, op.src2)
            is Operation.Or -> eval(ops, cache, op.src1) or eval(ops, cache, op.src2)
            is Operation.LeftShift -> eval(ops, cache, op.src) shl op.amt
            is Operation.RightShift -> eval(ops, cache, op.src) shr op.amt
        }
        val masked = r and 0xffff
        cache[expr] = masked
        return masked
    }
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
