import scala.io.Source
import scala.util.Using
import scala.sys.exit

object Program {
  def usage(): Unit = {
    Console.err.println("usage: scala Program <input file>")
    sys.exit(1)
  }

  def process(filename: String): Int = {
    val content = Using.resource(Source.fromFile(filename))(_.mkString)
    def checkPosition(chars: List[Char], floor: Int, pos: Int): Int = (chars, floor, pos) match {
      case (_, floor, pos) if floor < 0 => pos
      case ('('::xs, floor, pos) => checkPosition(xs, floor + 1, pos + 1)
      case (')'::xs, floor, pos) => checkPosition(xs, floor - 1, pos + 1)
      case (_::xs, floor, pos) => checkPosition(xs, floor, pos)
      case _ => 0
    }
    checkPosition(content.toList, 0, 0)
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
