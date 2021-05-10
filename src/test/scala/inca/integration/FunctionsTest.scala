package inca.integration

import inca.examples.functional.Code
import inca.frontend.functional.executor.FunctionalExecutor._
import org.scalatest.funsuite.AnyFunSuite

import scala.meta.XtensionQuasiquoteTerm

class FunctionsTest extends AnyFunSuite {
  test("Factorial Example") {
    val fun = loadFunction(Code.factModule)
    println(fun.compiled.optimized)
    assert(fun.execute("main", Seq(q"5")) == fun.resultVal(120))
    assert(fun.execute("main", Seq(q"10")) == fun.resultVal(3628800))
    fun.printAllMatches()
  }

  test("Fibonacci Example") {
    val fun = loadFunction(Code.fibModule)
    assert(fun.execute("main", Seq(q"10")) == fun.resultVal(55))
    assert(fun.execute("main", Seq(q"11")) == fun.resultVal(89))
    assert(fun.execute("main", Seq(q"20")) == fun.resultVal(6765))
    println(fun.compiled.optimized)
    fun.printAllMatches()
  }
}
