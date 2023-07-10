package inca.util

import java.io.{File, PrintWriter}

object FileUtil {
  def readFile(path: String): String = {
    val source = scala.io.Source.fromResource(path)
    val content = source.getLines().mkString("\n")
    source.close()
    content
  }

  def writeFile(path: String, content: String): Unit = {
    val file = new File(path)
    file.getParentFile.mkdirs()
    file.createNewFile()
    val writer = new PrintWriter(file)
    writer.write(content)
    writer.write("\n")
    writer.close()
  }
}
