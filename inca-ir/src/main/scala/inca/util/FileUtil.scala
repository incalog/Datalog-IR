package inca.util

import java.io.{File, FileWriter}

object FileUtil:
  def readLinesFromResource(path: String): Seq[String] =
    val source = scala.io.Source.fromResource(path)
    val lines = source.getLines().toSeq
    source.close()
    lines
  
  def readFileFromResource(path: String): String =
    val source = scala.io.Source.fromResource(path)
    val content = source.getLines().mkString("\n")
    source.close()
    content

  def readFile(path: String): String =
    val source = scala.io.Source.fromFile(path)
    val content = source.getLines().mkString("\n")
    source.close()
    content

  def writeFile(path: String, content: String): Unit =
    writeFile(new File(path), content)


  def writeFile(file: File, content: String): Unit =
    val fileWriter = new FileWriter(file)
    try
      fileWriter.write(content)
    finally
      fileWriter.close()

  def appendFile(path: String, content: String): Unit = ???
