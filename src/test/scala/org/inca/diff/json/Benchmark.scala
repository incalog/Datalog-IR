package org.inca.diff.json

import java.io.File

import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class Benchmark extends AnyFlatSpec {

  def readFile(path: String): String = {
    val source = Source.fromURL(getClass.getResource(path))
    val str = source.mkString
    source.close()
    str
  }

  def foreachFileLine(path: String)(f: String => Unit): Unit = {
    val source = Source.fromURL(getClass.getResource(path))
    for (line <- source.getLines())
      f(line)
    source.close()
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

  private def benchJsonFileByLine(path: String): Unit = {
    var i = 0
    var sizes: Int = 0
    var parses: Double = 0
    var diffs: Double = 0
    foreachFileLine(path) { line =>
      val (size, parse, diff) =benchJson(s"line$i of $path", line)
      sizes += size
      parses += parse
      diffs += diff
      i += 1
    }

    println(s"Benchmarking JSON document $path by line")
    println(s"  tree size: ${sizes.toDouble / i} nodes")
    println(s"  parsing: ${parses / i} ms")
    println(s"  diffing unchanged: ${diffs / i} ms")
  }

  private def benchJsonFile(path: String): Unit = {
    val (size, parse, diff) = benchJson(path, readFile(path))
    println(s"Benchmarking JSON document $path")
    println(s"  tree size: $size nodes")
    println(s"  parsing: $parse ms")
    println(s"  diffing unchanged: $diff ms")
  }

  private def benchJson(name: String, content: String): (Int, Double, Double) = {
    val tree = Parser.parse(content)
    val parseTime = timed(Parser.parse(content))
    val diffIdenticalTime = timed(tree.compareTo(tree))
    (tree.size, parseTime, diffIdenticalTime)
  }

  benchJsonFile("parboiled2bench.json")
//  benchJsonFile("canada.json")
  benchJsonFile("citm_catalog.json")
  benchJsonFile("twitter.json")
  benchJsonFileByLine("one-json-per-line.jsons")

}
