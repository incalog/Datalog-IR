package org.inca.diff.python

import java.io.File

import org.inca.diff.BenchmarkUtils._

import scala.collection.mutable

object BenchmarkCommits extends App {

  private var warmUp = false

  private var resultCount: Int = 0
  private var nodeCounts: Int = 0
  private var parsingTime: Double = 0
  private var diffingTime: Double = 0

  private def benchPythonFile(path: String, content: String, oldtree: Ast.file): Ast.file = {
    if (!warmUp) println(s"  Benchmarking Python document $path")
    try {
      val (tree, parse, diff) = benchPython(path, content, oldtree)
      resultCount += 1
      val nodeCount = tree.size
      nodeCounts += nodeCount
      if (parse.isNaN)
        throw new Exception(s"$path: $nodeCount, $parse, $diff")
      parsingTime += parse
      diffingTime += diff
//      if (!warmUp) println(s"    nodeCount: ${content.linesIterator.size} lines, $nodeCount nodes")
//      if (!warmUp) println(s"    parsing: $parse ms")
//      if (!warmUp) println(s"    diffing unchanged: $diff ms")
      tree
    } catch {
      case e: Exception if e.getMessage!=null && e.getMessage.startsWith("Parse Error") =>
        if (!warmUp) println(s"  parsing failed, skipping file")
        Ast.file.File(Seq())
    }
  }

  private def benchPython(name: String, content: String, oldtree: Ast.file): (Ast.file, Double, Double) = {
    val discard = if (warmUp) 1 else 0
    val repeat = if (warmUp) 0 else 10
    val (tree, parseTime) = timed(Statements.parse(content), discard, repeat)
    val (patch, diffIdenticalTime) = timed(oldtree.compareTo(tree))
    (tree, parseTime, diffIdenticalTime)
  }

  private val rootDir = new File("benchmark/python")
  private val djangoVersions = rootDir.listFiles()
    .filter(_.getName.startsWith("django-"))
    .sortBy(f=>f.getName.substring("django-".length, f.getName.lastIndexOf('-')).toInt)
    .reverse

  // warmup
  this.warmUp = true
  println(s"\nWarming up")
  foreachFile(djangoVersions.last.getAbsolutePath, pattern = ".*\\.py"){f =>
    try {
      val content = readFile(f)
      val tree = Statements.parse(content)
      benchPython(f, content, tree)
    } catch {case e:Exception if e.getMessage!=null && e.getMessage.startsWith("Parse Error") => }
  }

  // initial commit
  private val files = mutable.Map[String, (String, Ast.file)]()
  def trim(f: String) = f.substring(f.lastIndexOf('/')+1)

  foreachFile(djangoVersions.head.getAbsolutePath, pattern = ".*\\.py"){f =>
    val content = readFile(f)
    try {
      val ast = Statements.parse(content)
      files(trim(f)) = (content, ast)
    } catch {
      case e:Exception if e.getMessage.startsWith("Parse Error") =>
        files(trim(f)) = (content, Ast.file.File(Seq()))
    }
  }

  // benchmark
  this.warmUp = false
  println(s"\nBenchmarking")

  for (dir <- djangoVersions.tail) {
    println(s"\nBenchmark commit ${dir.getName}")
    val resultCount: Int = this.resultCount
    val nodeCounts: Int = this.nodeCounts
    val parsingTime: Double = this.parsingTime
    val diffingTime: Double = this.diffingTime

    val seen = mutable.Set[String]()
    foreachFile(dir.getAbsolutePath, pattern = ".*\\.py"){f =>
      seen += trim(f)
      val content = readFile(f)
      val (changed, oldAst) = files.get(trim(f)) match {
        case Some((oldContent, oldAst)) => (oldContent != content, oldAst)
        case None => (true, Ast.file.File(Seq()))
      }
      if (changed) {
        val tree = benchPythonFile(f, content, oldAst)
        files(trim(f)) = (content, tree)
      }
    }
    for (f <- files.keys if !seen.contains(f))
      files.remove(f)

    val commitResults = this.resultCount - resultCount
    if (commitResults > 0) {
      println(s"Commit Report")
      println(s"  parsed files: $commitResults")
      println(s"  average nodes in AST: ${(this.nodeCounts - nodeCounts).toDouble / commitResults}")
      println(s"  total parse time: ${this.parsingTime - parsingTime} ms")
      println(s"  average parse time: ${(this.parsingTime - parsingTime) / commitResults} ms")
      println(s"  total diff time: ${this.diffingTime - diffingTime} ms")
      println(s"  average diff time: ${(this.diffingTime - diffingTime) / commitResults} ms")
    }
  }


  // report
  println(s"\nReport")
  println(s"  parsed files: $resultCount")
  println(s"  average nodes in AST: ${nodeCounts.toDouble / resultCount}")
  println(s"  total parse time: ${parsingTime} ms")
  println(s"  average parse time: ${parsingTime / resultCount} ms")
  println(s"  total diff time: ${diffingTime} ms")
  println(s"  average diff time: ${diffingTime / resultCount} ms")
}
