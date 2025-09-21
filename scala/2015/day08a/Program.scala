import scala.io.Source
import scala.util.Using
import scala.sys.exit

object Program {
  def usage(): Unit = {
    Console.err.println("usage: scala Program <input file>")
    sys.exit(1)
  }

  def process(filename: String): Int = {
    Using(Source.fromFile(filename)) { source =>
      source.getLines().foldLeft(0) { (acc, line) =>
        def scanLine(i: Int, line: String): Int = {
          if ( line.length == 0 ) {
            i
          } else if ( line.length == 1 ) {
            i + 1
          } else {
            line(0) match {
              case '\\' =>
                line(1) match {
                  case '\\' | '"' => scanLine(i + 1, line.drop(2))
                  case 'x' => scanLine(i + 1, line.drop(4))
                  case _ => scanLine(i + 1, line.drop(1))
                }
              case _ => scanLine(i + 1, line.drop(1))
            }
          }
        }
        val codeLen = line.length
        val quoted = line.substring(1, line.length - 1)
        val memLen = scanLine(0, quoted)
        acc + (codeLen - memLen)
      }
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
