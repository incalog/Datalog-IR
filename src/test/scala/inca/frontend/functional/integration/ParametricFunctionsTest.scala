package inca.frontend.functional.integration

import inca.frontend.functional.executor.FunctionalExecutor
import inca.util.FileUtil
import org.scalatest.funsuite.AnyFunSuite

import scala.meta.XtensionQuasiquoteTerm

class ParametricFunctionsTest extends AnyFunSuite {
  test("parametric datatypes") {
    val code = FileUtil.readFile("functional/unittests/ParametricDatatypes.finca")
    val fun = FunctionalExecutor.loadFunction(code)
    // assert(fun.execute("main", Seq()) == fun.result(q"ConsBoolean$$0(true, NilBoolean$$0())"))
  }

  test("parametric functions") {
    val code = FileUtil.readFile("functional/unittests/ParametricFunction.finca")
    val fun = FunctionalExecutor.loadFunction(code)
    // assert(fun.execute("main", Seq()) == fun.resultVals(q"12", q"true"))
  }

  test("parametric eq") {
    val code = FileUtil.readFile("functional/unittests/ParametricEq.finca")
    val fun = FunctionalExecutor.loadFunction(code)
    assert(fun.execute("main", Seq(q"1", q"2")) == fun.result(q"false"))
    assert(fun.execute("main", Seq(q"3", q"3")) == fun.result(q"true"))
  }
}
