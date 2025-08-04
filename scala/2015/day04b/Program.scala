import scala.sys.exit

import java.security.MessageDigest

object Program {
  def usage(): Unit = {
    Console.err.println("usage: scala Program <input file>")
    sys.exit(1)
  }

  def process(key: String): Int = {
    val md5 = MessageDigest.getInstance("MD5")
    def checkKey(key: String, n: Int): Int = {
      val tryKey = key ++ n.toString
      val digestBytes = md5.digest(tryKey.getBytes("UTF-8"))
      val hexDigest = digestBytes.map("%02x".format(_)).mkString
      if ( hexDigest.startsWith("000000") ) n else checkKey(key, n + 1)
    }
    checkKey(key, 1)
  }

  def main(args: Array[String]): Unit = {
    args.toList match {
      case key :: Nil =>
        val result = process(key)
        println(s"result = $result")
      case _ =>
        usage()
    }
  }
}
