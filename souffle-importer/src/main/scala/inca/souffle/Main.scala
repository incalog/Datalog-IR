package inca.souffle

import inca.CompilerOptions
import inca.runtime.context.LanguageMetaInfo

import scala.collection.immutable.MultiDict
import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val filename = "souffle-importer/self-contained.dl"
    val file = Source.fromFile(filename).getLines.mkString("\n")
    val analysis = Parser(file)
    val compiler = new SouffleToIncaCompiler
    val incaModule = compiler.compile("self-contained", analysis)

    val languageMetaInfo = new LanguageMetaInfo(MultiDict(), Map(), compiler.genLitLinks)
    val psModule = inca.Compiler.compileAndLoadFunModule(incaModule, None, CompilerOptions(languageMetaInfo))

    val inputs = compiler.inputs
    inputs.foreach { case (rule, input) =>
      val sig = compiler.decls(rule)
      val inputCompiler = new SouffleInputToEditscript("souffle-importer/minijavac")
      val editScript = inputCompiler.compile(input, sig)
    }
  }
}