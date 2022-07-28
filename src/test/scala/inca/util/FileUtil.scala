package inca.util

object FileUtil {
  def readFile(path: String): String = {
    val source = scala.io.Source.fromResource(path)
    val content = source.getLines().mkString("\n")
    source.close()
    content
  }
}
