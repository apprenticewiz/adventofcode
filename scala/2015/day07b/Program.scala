import scala.collection.mutable.HashMap
import scala.io.Source
import scala.util.matching.Regex
import scala.util.Using
import scala.sys.exit

sealed trait Op

case class Assign(value: String) extends Op
case class Not(src: String) extends Op
case class And(src1: String, src2: String) extends Op
case class Or(src1: String, src2: String) extends Op
case class LeftShift(src: String, amt: Int) extends Op
case class RightShift(src: String, amt: Int) extends Op

object Program {
  def usage(): Unit = {
    Console.err.println("usage: scala Program <input file>")
    sys.exit(1)
  }

  def eval(operations: HashMap[String, Op], cache: HashMap[String, Int], value: String): Int = {
    if ( value.toIntOption.isDefined ) {
      value.toInt
    } else if ( cache.contains(value) ) {
      cache(value)
    } else {
      val result = operations(value) match {
        case Assign(x) => eval(operations, cache, x)
        case Not(x) =>  ~eval(operations, cache, x)
        case And(x1, x2) => eval(operations, cache, x1) & eval(operations, cache, x2)
        case Or(x1, x2) => eval(operations, cache, x1) | eval(operations, cache, x2)
        case LeftShift(x, amt) => eval(operations, cache, x) << amt
        case RightShift(x, amt) => eval(operations, cache, x) >>> amt
      }
      val masked = result & 0xffff
      cache += (value -> masked)
      masked
    }
  }

  def process(filename: String): Int = {
    val operations: HashMap[String, Op] = new HashMap
    val assignRegex: Regex = raw"(\d+|\w+) -> (\w+)".r
    val notRegex: Regex = raw"NOT (\d+|\w+) -> (\w+)".r
    val andOrRegex: Regex = raw"(\d+|\w+) (AND|OR) (\d+|\w+) -> (\w+)".r
    val shiftRegex: Regex = raw"(\d+|\w+) (LSHIFT|RSHIFT) (\d+) -> (\w+)".r
    Using(Source.fromFile(filename)) { source =>
      for ( line <- source.getLines() ) {
        line match {
          case assignRegex(value, dest) => operations += (dest -> Assign(value))
          case notRegex(src, dest) => operations += (dest -> Not(src))
          case andOrRegex(src1, op, src2, dest) =>
            operations += (dest -> (if ( op == "AND" ) And(src1, src2) else Or(src1, src2)))
          case shiftRegex(src, op, amtStr, dest) =>
            val amt = amtStr.toInt
            operations += (dest -> (if (op == "LSHIFT" ) LeftShift(src, amt) else RightShift(src, amt)))
          case _ =>
            Console.err.println(s"error: malformed input line: $line")
            sys.exit(1)
        }
      }
    }
    val a = eval(operations, new HashMap, "a")
    operations("b") = Assign(a.toString)
    eval(operations, new HashMap, "a")
  }

  def main(args: Array[String]): Unit = {
    args.toList match {
      case filename :: Nil =>
        val result = process(filename)
        println(s"result = $result")
      case _ =>
        usage()
    }
  }
}
