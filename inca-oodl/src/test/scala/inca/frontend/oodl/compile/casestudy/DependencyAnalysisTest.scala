package inca.frontend.oodl.compile.casestudy

import inca.frontend.oodl.compile.{CompiledOODLUnit, OODLCompilerOptions}
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
    val unit = CompiledOODLUnit(module, OODLCompilerOptions.default)
    unit.setPipeline(CompiledOODLUnit.pipeline)
    unit.setOptimizationPipeline(CompiledOODLUnit.optimizationPipeline)
    unit.compiled

  val file = Source.fromURI(oodlFile)
  val sourceCode = file.getLines().mkString("\n")
  file.close()
  testCompile(sourceCode)

