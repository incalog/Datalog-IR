package inca.frontend.functional.integration

import inca.frontend.functional.executor.FunctionalExecutor._
import inca.compiler.CompiledModule
import inca.util.FileUtil
import org.scalatest.funsuite.AnyFunSuite

import scala.meta.XtensionQuasiquoteTerm

class HigherOrderTest extends AnyFunSuite {

  test("applyFun") {
    val code = FileUtil.readFile("functional/higherorder/Apply.finca")
    val fun = loadFunction(code)
    assert(fun.execute("main", Seq()) == fun.result(q"6"))
  }

  test("lambda") {
    val code = FileUtil.readFile("functional/higherorder/Lambda.finca")
    val fun = loadFunction(code)
    assert(fun.execute("main", Seq()) == fun.result(q"21"))
  }

  test("lambdaHigherOrder") {
    val code = FileUtil.readFile("functional/higherorder/LambdaHigherOrder.finca")
    val fun = loadFunction(code)
    assert(fun.execute("main", Seq()) == fun.result(q"63"))
  }

  test("composeFun") {
    val code = FileUtil.readFile("functional/higherorder/ComposeFun.finca")
    val fun = loadFunction(code)
    assert(fun.execute("main", Seq()) == fun.result(meta.Lit.String(Math.sqrt(2).toString)))
  }

  test("composeLambdas") {
    val code = FileUtil.readFile("functional/higherorder/ComposeLambda.finca")
    val fun = loadFunction(code)
    assert(fun.execute("main", Seq()) == fun.result(meta.Lit.String(Math.sqrt(2).toString)))
  }

  test("transitiveWrong") {
    val code = FileUtil.readFile("functional/higherorder/TransitiveWrong.finca")
    assertThrows[CompiledModule.Failed](loadFunction(code))
  }

  test("transitive") {
    val code = FileUtil.readFile("functional/higherorder/Transitive.finca")
    val fun = loadFunction(code)
    assert(fun.execute("foo", Seq()) ==
      fun.results(
        Seq(Seq(1,2), Seq(2,3), Seq(3,1), Seq(1,3), Seq(1,1), Seq(2,1), Seq(2,2), Seq(3,2), Seq(3,3))))
  }
}
