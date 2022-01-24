package inca.frontend.functional.debugger

import inca.analyzedLangs.{Exp, ExpLangTestAnalyses}
import inca.backend.ir.Datalog
import inca.compiler.Compiler
import inca.debugger.ScalaValue
import inca.debugger.table.Table
import inca.examples.functional.{AST, Code}
import inca.frontend.functional.compiler.FunctionalOptions
import inca.runtime.context.DataModel
import inca.util.Scala
import org.scalatest.funsuite.AnyFunSuite
import truechange.EditScript
import truediff.Diffable

import scala.meta.XtensionQuasiquoteTerm

class FunctionalDebuggerTest extends AnyFunSuite {
  def initDebugger(module: Datalog.Module, dataModel: DataModel, tree: Diffable): FunctionalDebugger =
    initDebugger(module, dataModel, tree.loadEdits)

  def stepTillFinish(debugger: FunctionalDebugger): Unit = {
    while (!debugger.isFinished)
      debugger.stepInto()
  }

  def initDebugger(module: Datalog.Module, dataModel: DataModel, edits: EditScript = EditScript(Seq())): FunctionalDebugger = {
    val debugger = new FunctionalDebugger
    debugger.initialize(module, dataModel, edits)
    debugger
  }

  test("if example control") {
    val compiledExample = Compiler.compileFunctional(Code.ifExample, FunctionalOptions())
    val debugger = initDebugger(compiledExample.ir, new DataModel())
    debugger.entry("main", Table.unit)
    while (!debugger.isFinished) {
      debugger.currentCodeFunction.lines().map("  |  " + _).forEach(println)
      println(debugger.currentBindings)
      debugger.stepIntoFrontend()
    }
    println(debugger.relation("main"))
  }

  test("if example 2 control") {
    val compiledExample = Compiler.compileFunctional(Code.ifExample2, FunctionalOptions())
    val debugger = initDebugger(compiledExample.ir, new DataModel())
    debugger.entry("main", Table.unit)
    while (!debugger.isFinished) {
      debugger.currentCodeFunction.lines().map("  |  " + _).forEach(println)
      println(debugger.currentBindings)
      debugger.stepIntoFrontend()
    }
    println(debugger.relation("main"))
  }

  def ifControlJump(b1: Boolean, b2: Boolean): String =
    s"""module M
       |@main def main(): Int =
       |  if ($b1 == true)
       |    if ($b2 == true)
       |      0 + 0
       |    else
       |      1 + 0
       |  else
       |    if ($b2 == true)
       |      2 + 0
       |    else
       |      3 + 0
       |""".stripMargin

  test("if control jumping") {
    for (b1 <- Seq(true, false); b2 <- Seq(true, false)) {
      val compiledExample = Compiler.compileFunctional(ifControlJump(b1, b2), FunctionalOptions())
      val debugger = initDebugger(compiledExample.ir, new DataModel())
      debugger.entry("main", Table.unit)
      debugger.untilFinished(() => debugger.stepIntoFrontend())
      assertResult(5)(debugger.controlTraceFrontend.size)
    }
  }

  test("fib example control") {
    val compiledExample = Compiler.compileFunctional(Code.fibModule, FunctionalOptions())
    val debugger = initDebugger(compiledExample.ir, new DataModel())
    debugger.entry("main", Table(Map("x" -> ScalaValue(3))))
    while (!debugger.isFinished) {
      println(debugger.currentCallStack)
      println("  " + debugger.currentBindings)
      debugger.currentCodeFunction.lines().map("  |  " + _).forEach(println)
      debugger.stepIntoFrontend()
    }
    println(debugger.relation("main"))
  }
}
