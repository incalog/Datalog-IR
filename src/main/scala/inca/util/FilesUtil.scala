package inca.util

import java.io.File
import java.io.PrintWriter
import scala.io.Source

object FilesUtil {
  def readFile(path: String): String = {
    val source = Source.fromFile(path)
    val str = source.mkString
    source.close()
    str
  }

  def writeFile(path: String, content: String): Unit = {
    val file = new File(path)
    file.getParentFile.mkdirs()
    file.createNewFile()
    val writer = new PrintWriter(file)
    writer.write(content)
    writer.close()
  }

  def foreachFileLine(path: String)(f: String => Unit): Unit = {
    val source = Source.fromFile(path)
    for (line <- source.getLines())
      f(line)
    source.close()
  }

  def files(path: String, transitive: Boolean = true, pattern: String = ".*"): Seq[File] = {
    val file = new File(path)
    if (file.isDirectory) {
      file.listFiles().toList.flatMap { sub =>
        val subpath = s"$path/${sub.getName}"
        if (sub.isFile && sub.getName.matches(pattern)) Seq(sub)
        else if (transitive && sub.isDirectory)
          files(subpath, transitive, pattern)
        else Nil
      }
    } else Nil
  }

  def foreachFile(
      path: String,
      transitive: Boolean = true,
      pattern: String = ".*"
    )(
      f: String => Unit
    ): Unit = {
    val file = new File(path)
    if (file.isDirectory) {
      file.listFiles().foreach { sub =>
        val subpath = s"$path/${sub.getName}"
        if (sub.isFile && sub.getName.matches(pattern))
          f(subpath)
        else if (transitive && sub.isDirectory)
          foreachFile(subpath, transitive, pattern)(f)
      }
    }
  }
}
