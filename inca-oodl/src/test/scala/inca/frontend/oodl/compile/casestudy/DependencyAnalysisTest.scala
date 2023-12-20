package inca.frontend.oodl.compile.casestudy

import inca.frontend.oodl.compile.CompiledOODLModule
import inca.frontend.oodl.typechecker.{Typechecker, TypecheckerTest}
import org.scalatest.funsuite.AnyFunSuite
import inca.frontend.oodl.syntax.Parser
<<<<<<< HEAD
import inca.util.compileroptions.CompilerOptions
=======
>>>>>>> layered-ir-setfold-oodl

import scala.io.Source

class DependencyAnalysisTest extends AnyFunSuite:

  val oodlFile = classOf[DependencyAnalysisTest].getResource("/objectoriented/casestudies/DependencyAnalysis.oodl").toURI

  def testCompile(code: String): Unit =
    val checker = new Typechecker
    val module = Parser.parseModule(code)
    //println(module)
    //checker.typecheck(module)
    //checker.printTypeIO()
    val compiled = CompiledOODLModule(module, CompilerOptions.default)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    compiled.lowered

  val file = Source.fromURI(oodlFile)
  val sourceCode = file.getLines().mkString("\n")
  file.close()
  testCompile(sourceCode)

