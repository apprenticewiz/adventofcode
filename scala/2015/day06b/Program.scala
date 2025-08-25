import scala.io.Source
import scala.util.matching.Regex
import scala.util.Using
import scala.sys.exit

object Program {
  def usage(): Unit = {
    Console.err.println("usage: scala Program <input file>")
    sys.exit(1)
  }

  def perform(grid: Array[Int], action: String, r1: Int, c1: Int, r2: Int, c2: Int) = {
    for (row <- r1 to r2) {
      for (col <- c1 to c2) {
        action match {
          case "turn on" => grid(row * 1000 + col) += 1
          case "turn off" => grid(row * 1000 + col) = math.max(0, grid(row * 1000 + col) - 1)
          case "toggle" => grid(row * 1000 + col) += 2
          case _ => ()
        }
      }
    }
  }

  def process(filename: String): Int = {
    val re: Regex = raw"(turn on|turn off|toggle) (\d+),(\d+) through (\d+),(\d+)".r
    val grid: Array[Int] = Array.fill(1000*1000)(0)
    Using(Source.fromFile(filename)) { source =>
      source.getLines().foreach({ line =>
        re.findFirstMatchIn(line) match {
          case Some(matches) =>
             val action = matches.group(1)
             val r1 = matches.group(2).toInt
             val c1 = matches.group(3).toInt
             val r2 = matches.group(4).toInt
             val c2 = matches.group(5).toInt
             perform(grid, action, r1, c1, r2, c2)
          case None => ()
        }
      })
    }
    grid.sum
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
