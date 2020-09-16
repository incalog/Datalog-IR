package inca.souffle

import java.io.{BufferedWriter, File, FileWriter}

import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val filename = "souffle-importer/self-contained.dl"
    println(System.getProperty("user.dir"))
    val file = Source.fromFile(filename).getLines.mkString("\n")
    val analysis = Parser(file)
    analysis.contents.foreach(println)
    val compiler = new SouffleToIncaCompiler
    val incaModule = compiler.compile("self-contained", analysis)
//    compiler.types.foreach(println)
//    compiler.caseClasses.foreach(println)
//    println(incaModule.prettyprint(""))

    val factObject = compiler.compileScalaFile
    val factsFile = new File("souffle-importer/src/main/scala/inca/souffle/Facts.scala")
    val bw = new BufferedWriter(new FileWriter(factsFile))
    bw.write(factObject.syntax)
    bw.close()
//    println(factFile.syntax)
  }
}