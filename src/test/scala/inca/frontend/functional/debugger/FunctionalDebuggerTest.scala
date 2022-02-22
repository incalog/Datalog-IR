package inca.frontend.functional.debugger

import inca.compiler.Compiler
import inca.examples.functional.{ADT, Code}
import inca.frontend.functional.compiler.{CompiledFunctionalModule, FunctionalOptions}
import org.scalatest.funsuite.AnyFunSuite
import meta.quasiquotes._

class FunctionalDebuggerTest extends AnyFunSuite {

  def initDebugger(module: CompiledFunctionalModule): FunctionalDebugger = {
    new FunctionalDebugger(module)
  }

  test("if example") {
    val compiledExample = Compiler.compileFunctional(Code.ifExample, FunctionalOptions())
    val debugger = initDebugger(compiledExample)
    debugger.entry("main")
    while (!debugger.isFinished) {
      println(debugger.currentDebuggerInfo)
      debugger.stepInto()
    }
    println(debugger.relation("main"))
  }

  test("if example 2") {
    val compiledExample = Compiler.compileFunctional(Code.ifExample2, FunctionalOptions())
    val debugger = initDebugger(compiledExample)
    debugger.entry("main")
    while (!debugger.isFinished) {
      println(debugger.currentDebuggerInfo)
      debugger.stepInto()
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
      debugger.entry("main")
      while (!debugger.isFinished) {
        println(debugger.currentDebuggerInfo)
        debugger.stepInto()
      }
      assertResult(5)(debugger.controlTraceFrontend.size)
    }
  }

  test("fib example") {
    val compiledExample = Compiler.compileFunctional(Code.fibModule, FunctionalOptions())
    val debugger = initDebugger(compiledExample)
    debugger.entry("main", q"3")
    while (!debugger.isFinished) {
      println(debugger.currentDebuggerInfo)
      debugger.stepInto()
    }
    println(debugger.relation("main"))
  }

  test("Constructor calls example") {
    val code = Code.module(
      ADT.Nat_code,
      """@main def main(): Nat = Succ(Succ(Zero()))
        |""".stripMargin
    )
    val compiledExample = Compiler.compileFunctional(code, FunctionalOptions())
    val debugger = initDebugger(compiledExample)
    debugger.entry("main")
    while (!debugger.isFinished) {
      println(debugger.currentDebuggerInfo)
      debugger.stepInto()
    }
    println(debugger.relation("main"))
  }

  test("plus example") {
    val compiledExample = Compiler.compileFunctional(Code.plusRealModule, FunctionalOptions())
    val debugger = initDebugger(compiledExample)
    debugger.entry("main", q"Succ(Succ(Zero()))", q"Succ(Zero())")
    while (!debugger.isFinished) {
      println(debugger.currentDebuggerInfo)
      debugger.stepInto()
    }
    println(debugger.relation("main"))
  }

  test("plus example extra") {
    val compiledExample = Compiler.compileFunctional(Code.plusRealModuleExtra, FunctionalOptions())
    val debugger = initDebugger(compiledExample)
    debugger.entry("main", q"Succ(Succ(Zero()))", q"Succ(Zero())")
    while (!debugger.isFinished) {
      println(debugger.currentDebuggerInfo)
      debugger.stepInto()
    }
    println(debugger.relation("main"))
  }
}
