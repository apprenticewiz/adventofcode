import scala.collection.immutable.HashSet
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
    content.toList.foldLeft(((0, 0), HashSet[(Int, Int)]())) {
      (acc : ((Int, Int), HashSet[(Int, Int)]), ch : Char) =>
        val (santa, positions) = acc
        val nextSanta = ch match {
          case '^' => (santa._1, santa._2 + 1)
          case 'v' => (santa._1, santa._2 - 1)
          case '<' => (santa._1 - 1, santa._2)
          case '>' => (santa._1 + 1, santa._2)
          case _ => santa
        }
        val nextPositions = positions + nextSanta
        (nextSanta, nextPositions)
    }._2.size
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
