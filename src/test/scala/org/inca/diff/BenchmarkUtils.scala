package org.inca.diff

import java.io.File
import java.nio.file.{Files, Path, Paths}

import scala.io.Source

object BenchmarkUtils {
  def readFile(path: String): String = {
    val source = Source.fromFile(path)
    val str = source.mkString
    source.close()
    str
  }

  def foreachFileLine(path: String)(f: String => Unit): Unit = {
    val source = Source.fromFile(path)
    for (line <- source.getLines())
      f(line)
    source.close()
  }

  def foreachFile(path: String, transitive: Boolean = true, pattern: String = ".*")(f: String => Unit): Unit = {
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

  def ms(l: Double): Double = l/1000/1000

  def time[R](block: => R): (R,Long) = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    (result, t1 - t0)
  }

  def timed[R](block: => R, discard: Int = 10, repeat: Int = 10): (R, Double) = {
    var result = null.asInstanceOf[R]

    // discard first 10 runs
    for (_ <- 1 to discard) {
      val (r,_) = time(block)
      result = r
    }

    var sum: Long = 0
    for (_ <- 1 to repeat) {
      val (r, t) = time(block)
      result = r
      sum += t
    }
    (result, if (repeat == 0) 0 else ms(sum.toDouble / repeat))
  }
}
