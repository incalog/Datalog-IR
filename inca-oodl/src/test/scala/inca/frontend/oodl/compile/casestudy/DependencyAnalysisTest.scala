package inca.frontend.oodl.compile.casestudy

import inca.frontend.oodl.compile.{CompiledOODLModule, OODLCompilerOptions}
import inca.frontend.oodl.typechecker.{Typechecker, TypecheckerTest}
import org.scalatest.funsuite.AnyFunSuite
import inca.frontend.oodl.syntax.Parser

import scala.io.Source

class DependencyAnalysisTest extends AnyFunSuite:

  val oodlFile = classOf[DependencyAnalysisTest].getResource("/objectoriented/casestudies/DependencyAnalysis.oodl").toURI

  def testCompile(code: String): Unit =
    val checker = new Typechecker
    val module = Parser.parseModule(code)
    //println(module)
    //checker.typecheck(module)
    //checker.printTypeIO()
    val compiled = CompiledOODLModule(module, OODLCompilerOptions.default)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    compiled.lowered

  val file = Source.fromURI(oodlFile)
  val sourceCode = file.getLines().mkString("\n")
  file.close()
  testCompile(sourceCode)

