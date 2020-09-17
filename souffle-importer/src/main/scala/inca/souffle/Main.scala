package inca.souffle

import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val filename = "souffle-importer/self-contained.dl"
    val file = Source.fromFile(filename).getLines.mkString("\n")
    val analysis = Parser(file)
    val compiler = new SouffleToIncaCompiler
    val incaModule = compiler.compile("self-contained", analysis)
//    println(incaModule.prettyprint(""))

    val inputs = compiler.inputs
    inputs.headOption.foreach { case (rule, input) =>
      val sig = compiler.decls(rule)
      val inputCompiler = new SouffleInputToEditscript("souffle-importer/minijavac")
      val editScript = inputCompiler.compile(input, sig)
//      editScript.foreach(println)
    }
  }
}