package inca.frontend.functional.debugger

import inca.analyzedLangs.{Exp, ExpLangTestAnalyses}
import inca.backend.ir.Datalog
import inca.compiler.Compiler
import inca.debugger.table.Table
import inca.examples.functional.AST
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
    val compiledExample = Compiler.compileFunctional(AST.ifExample, FunctionalOptions())
    println(compiledExample.ir)
    println(compiledExample.fun)
    val debugger = initDebugger(compiledExample.ir, new DataModel())
    debugger.entry("main", Table.unit)
    while (!debugger.isFinished) {
      println(s"${debugger.controlPointFrontend}:\n  ${debugger.varsFrontEnd}")
      debugger.stepIntoFrontend()
    }
    debugger.controlTraceFrontend.foreach(println)
    println(debugger.relation("main"))
  }

  test("if example 2 control") {
    val compiledExample = Compiler.compileFunctional(AST.ifExample2, FunctionalOptions())
    println(compiledExample.ir)
    val debugger = initDebugger(compiledExample.ir, new DataModel())
    debugger.entry("main", Table.unit)
    while (!debugger.isFinished) {
      println(s"${debugger.controlPointFrontend}:\n  ${debugger.varsFrontEnd}")
      debugger.stepIntoFrontend()
    }
    debugger.controlTraceFrontend.foreach(println)
    println(debugger.relation("main"))
  }
}
