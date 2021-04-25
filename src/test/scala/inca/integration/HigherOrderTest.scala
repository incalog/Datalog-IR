package inca.integration

import inca.Executor._
import inca.compiler.CompiledModule
import inca.examples.HigherOrder
import org.scalatest.funsuite.AnyFunSuite

import scala.meta.XtensionQuasiquoteTerm

class HigherOrderTest extends AnyFunSuite {

  test("applyFun") {
    val fun = loadFunction(HigherOrder.applyFun)
    assert(fun.execute("main", Seq()) == fun.result(q"6"))
    fun.printAllMatches()
  }

  test("lambda") {
    val fun = loadFunction(HigherOrder.lambda)
    assert(fun.execute("main", Seq()) == fun.result(q"21"))
    fun.printAllMatches()
  }

  test("lambdaHigherOrder") {
    val fun = loadFunction(HigherOrder.lambdaHigherOrder)
    assert(fun.execute("main", Seq()) == fun.result(q"63"))
    fun.printAllMatches()
  }

  test("composeFun") {
    val fun = loadFunction(HigherOrder.composeFun)
    assert(fun.execute("main", Seq()) == fun.result(meta.Lit.String(Math.sqrt(2).toString)))
    fun.printAllMatches()
  }

  test("composeLambdas") {
    val fun = loadFunction(HigherOrder.composeLambdas)
    assert(fun.execute("main", Seq()) == fun.result(meta.Lit.String(Math.sqrt(2).toString)))
    fun.printAllMatches()
  }

  test("transitiveWrong") {
    assertThrows[CompiledModule.Failed](loadFunction(HigherOrder.transitiveWrong))
  }

  test("transitive") {
    val fun = loadFunction(HigherOrder.transitive)
    assert(fun.execute("foo", Seq()) ==
      fun.results(
        Seq(Seq(1,2), Seq(2,3), Seq(3,1), Seq(1,3), Seq(1,1), Seq(2,1), Seq(2,2), Seq(3,2), Seq(3,3))))
    fun.printAllMatches()
  }
}
