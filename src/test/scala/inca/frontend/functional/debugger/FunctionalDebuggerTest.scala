package inca.frontend.functional.debugger

import inca.compiler.Compiler
import inca.debugger.ScalaValue
import inca.debugger.table.Table
import inca.examples.functional.Code
import inca.frontend.functional.compiler.{CompiledFunctionalModule, FunctionalOptions}
import org.scalatest.funsuite.AnyFunSuite
import truechange.EditScript

class FunctionalDebuggerTest extends AnyFunSuite {

  def initDebugger(module: CompiledFunctionalModule, edits: EditScript = EditScript(Seq())): FunctionalDebugger = {
    val debugger = new FunctionalDebugger
    debugger.initialize(module, edits)
    debugger
  }

  test("if example") {
    val compiledExample = Compiler.compileFunctional(Code.ifExample, FunctionalOptions())
    val debugger = initDebugger(compiledExample)
    debugger.entry("main", Table.unit)
    while (!debugger.isFinished) {
      debugger.currentCodeFunction.lines().map("  |  " + _).forEach(println)
      println(debugger.currentBindings)
      debugger.stepIntoFrontend()
    }
    println(debugger.relation("main"))
  }

  test("if example 2") {
    val compiledExample = Compiler.compileFunctional(Code.ifExample2, FunctionalOptions())
    val debugger = initDebugger(compiledExample)
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
      val debugger = initDebugger(compiledExample)
      debugger.entry("main", Table.unit)
      debugger.untilFinished(() => debugger.stepIntoFrontend())
      assertResult(5)(debugger.controlTraceFrontend.size)
    }
  }

  test("fib example") {
    val compiledExample = Compiler.compileFunctional(Code.fibModule, FunctionalOptions())
    val debugger = initDebugger(compiledExample)
    debugger.entry("main", Table(Map("x" -> ScalaValue(3))))
    while (!debugger.isFinished) {
      println(debugger.currentCallStack)
      println("  " + debugger.currentBindings)
      debugger.currentCodeFunction.lines().map("  |  " + _).forEach(println)
      debugger.stepIntoFrontend()
    }
    println(debugger.relation("main"))
  }

  test("plus example") {
    val compiledExample = Compiler.compileFunctional(Code.plusRealModule, FunctionalOptions())
    val debugger = initDebugger(compiledExample)
    debugger.entry("main", Table(Map("x" -> ScalaValue(3), "y" -> ScalaValue(3))))
    while (!debugger.isFinished) {
      println(debugger.currentCallStack)
      println("  " + debugger.currentBindings)
      debugger.currentCodeFunction.lines().map("  |  " + _).forEach(println)
      debugger.stepIntoFrontend()
    }
    println(debugger.relation("main"))
  }
}
