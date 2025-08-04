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
    content.toList.foldLeft(((0, 0), (0, 0), HashSet[(Int, Int)](), false)) {
      (acc : ((Int, Int), (Int, Int), HashSet[(Int, Int)], Boolean), ch : Char) =>
        val (santa, roboSanta, positions, santaMove) = acc
        val nextPosition = santaMove match {
          case true => ch match {
            case '^' => (santa._1, santa._2 + 1)
            case 'v' => (santa._1, santa._2 - 1)
            case '<' => (santa._1 - 1, santa._2)
            case '>' => (santa._1 + 1, santa._2)
            case _ => santa
          }
          case false => ch match {
            case '^' => (roboSanta._1, roboSanta._2 + 1)
            case 'v' => (roboSanta._1, roboSanta._2 - 1)
            case '<' => (roboSanta._1 - 1, roboSanta._2)
            case '>' => (roboSanta._1 + 1, roboSanta._2)
            case _ => roboSanta
          }
        }
        val nextSanta = if ( santaMove ) nextPosition else santa
        val nextRoboSanta = if ( santaMove ) roboSanta else nextPosition
        val nextPositions = positions + nextPosition
        val nextMove = !santaMove
        (nextSanta, nextRoboSanta, nextPositions, nextMove)
    }._3.size
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
