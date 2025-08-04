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
        val area1 = l * w
        val area2 = l * h
        val area3 = w * h
        val surfaceArea = 2 * area1 + 2 * area2 + 2 * area3
        val minArea = List(area1, area2, area3).min
        acc + surfaceArea + minArea
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
