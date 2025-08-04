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
    content.foldLeft(0) {
      case (floor, '(') => floor + 1
      case (floor, ')') => floor - 1
      case (floor, _) => floor
    }
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
