package inca.souffle

import inca.CompilerOptions
import inca.runtime.Database
import inca.runtime.context.LanguageMetaInfo

import scala.collection.immutable.MultiDict
import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val filename = "souffle-importer/self-contained.dl"
    val file = Source.fromFile(filename).getLines.mkString("\n")
    val analysis = Parser(file)
    val compiler = new SouffleToIncaCompiler
    val (gpModule, inputs, languageMetaInfo)  = compiler.compile("self-contained", analysis)


    println(gpModule)
    val psModule = inca.Compiler.compileAndLoadGPModule(gpModule, None, CompilerOptions(languageMetaInfo))

//    println(compiler.patFuns("Method_Descriptor").prettyprint(""))
//    val inputs = compiler.inputs
//    val database = new Database
//    inputs.headOption.foreach { case (sig, input) =>
//      val inputCompiler = new SouffleInputToEditscript("souffle-importer/minijavac")
//      val editScript = inputCompiler.compile(input, sig)
//      println(editScript.size)
//      database.processEditScript(editScript)
//    }

  }
}