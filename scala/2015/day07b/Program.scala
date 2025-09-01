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
    val assignHandler: List[String] => Unit = groups => {
      val List(src, dest) = groups
      operations += (dest -> Assign(src))
    }
    val notHandler: List[String] => Unit = groups => {
      val List(src, dest) = groups
      operations += (dest -> Not(src))
    }
    val andOrHandler: List[String] => Unit = groups => {
      val List(src1, op, src2, dest) = groups
      operations += (dest -> (if (op == "AND") And(src1, src2) else Or(src1, src2)))
    }
    val shiftHandler: List[String] => Unit = groups => {
      val List(src, op, amt, dest) = groups
      operations += (dest -> (if (op == "LSHIFT") LeftShift(src, amt.toInt) else RightShift(src, amt.toInt)))
    }
    val regexes: List[(Regex, List[String] => Unit)] = List(
      (raw"(\d+|\w+) -> (\w+)".r, assignHandler),
      (raw"NOT (\d+|\w+) -> (\w+)".r, notHandler),
      (raw"(\d+|\w+) (AND|OR) (\d+|\w+) -> (\w+)".r, andOrHandler),
      (raw"(\d+|\w+) (LSHIFT|RSHIFT) (\d+) -> (\w+)".r, shiftHandler),
    )
    Using(Source.fromFile(filename)) { source =>
      for ( line <- source.getLines() ) {
        val matched = regexes.exists { case(re, handler) =>
          re.unapplySeq(line) match {
            case Some(groups) =>
              handler(groups)
              true
            case None =>
              false
          }
        }
        if ( !matched ) {
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
