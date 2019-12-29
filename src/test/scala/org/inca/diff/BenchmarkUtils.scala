package org.inca.diff

import java.io.File
import java.nio.file.{Files, Path, Paths}

import scala.io.Source

object BenchmarkUtils {
  def readRessourceFile(who: Any, path: String): String = {
    val source = Source.fromURL(who.getClass.getResource(path))
    val str = source.mkString
    source.close()
    str
  }

  def readFile(path: String): String = {
    val source = Source.fromFile(path)
    val str = source.mkString
    source.close()
    str
  }

  def foreachFileLine(who: Any, path: String)(f: String => Unit): Unit = {
    val source = Source.fromURL(who.getClass.getResource(path))
    for (line <- source.getLines())
      f(line)
    source.close()
  }

  def foreachFile(path: String, transitive: Boolean = true, pattern: String = ".*")(f: String => Unit): Unit = {
    val file = new File(path)
    if (file.isDirectory) {
      file.listFiles().foreach { sub =>
        if (sub.isFile && sub.getName.matches(pattern))
          f(sub.getAbsolutePath)
        else if (transitive && sub.isDirectory)
          foreachFile(sub.getAbsolutePath, transitive, pattern)(f)
      }
    }
  }

  def ms(l: Double): Double = l/1000/1000

  def time[R](block: => R): Long = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    t1 - t0
  }

  def timed[R](block: => R, discard: Int = 10, repeat: Int = 10): Double = {
    // discard first 10 runs
    for (_ <- 1 to discard)
      time(block)

    var sum: Long = 0
    for (_ <- 1 to repeat)
      sum += time(block)
    ms(sum.toDouble / repeat)
  }
}
