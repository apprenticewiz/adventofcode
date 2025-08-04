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
        val dims: List[Int] = line.split('x').toList.map(_.toInt)
        val (l, w, h): (Int, Int, Int) = (dims(0), dims(1), dims(2))
        val perim1 = 2 * (l + w)
        val perim2 = 2 * (l + h)
        val perim3 = 2 * (w + h)
        val presentLen = List(perim1, perim2, perim3).min
        val bowLen = l * w * h
        acc + presentLen + bowLen
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
