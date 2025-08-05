import scala.io.Source
import scala.util.Using
import scala.sys.exit

object Program {
  def usage(): Unit = {
    Console.err.println("usage: scala Program <input file>")
    sys.exit(1)
  }

  def process(filename: String): Int = {
    def prop1(str: String) : Boolean = {
      "(..).*\\1".r.findFirstIn(str).isDefined
    }

    def prop2(str: String) : Boolean = {
      "(.).\\1".r.findFirstIn(str).isDefined
    }

    Using(Source.fromFile(filename)) { source =>
      source.getLines().count(s => prop1(s) && prop2(s))
    }.getOrElse(0)
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
