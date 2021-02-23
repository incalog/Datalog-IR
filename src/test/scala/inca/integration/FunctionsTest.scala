package inca.integration

import inca.Executor._
import inca.examples.Code
import org.scalatest.funsuite.AnyFunSuite

import scala.meta.XtensionQuasiquoteTerm

class FunctionsTest extends AnyFunSuite {
  test("Factorial Example") {
    val fun = loadFunction(Code.factModule)
    println(fun.compiled.optimized)
    assert(fun.execute("main_bf", Seq(q"5")) == fun.resultVal(120))
    assert(fun.execute("main_bf", Seq(q"10")) == fun.resultVal(3628800))
    fun.printAllMatches()
  }

  test("Fibonacci Example") {
    val fun = loadFunction(Code.fibModule)
    assert(fun.execute("main_bf", Seq(q"10")) == fun.resultVal(55))
    assert(fun.execute("main_bf", Seq(q"11")) == fun.resultVal(89))
    assert(fun.execute("main_bf", Seq(q"20")) == fun.resultVal(6765))
    fun.printAllMatches()
  }
}
