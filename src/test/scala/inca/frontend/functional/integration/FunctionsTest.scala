package inca.frontend.functional.integration

import inca.frontend.functional.compiler.FunctionalOptions
import inca.frontend.functional.executor.FunctionalExecutor
import inca.util.FileUtil.readFile
import org.scalatest.funsuite.AnyFunSuite
import scala.meta.XtensionQuasiquoteTerm

class FunctionsTest extends AnyFunSuite {
  test("Factorial Example") {
    val code = readFile("functional/unittests/Fact.finca")
    val fun = FunctionalExecutor.loadFunction(code)
    assert(fun.execute("main", Seq(q"5")) == fun.resultVal(120))
    assert(fun.execute("main", Seq(q"10")) == fun.resultVal(3628800))
  }

  test("Fibonacci Example") {
    val code = readFile("functional/unittests/Fib.finca")
    val fun = FunctionalExecutor.loadFunction(code)
    assert(fun.execute("main", Seq(q"10")) == fun.resultVal(55))
    assert(fun.execute("main", Seq(q"11")) == fun.resultVal(89))
    assert(fun.execute("main", Seq(q"20")) == fun.resultVal(6765))
  }

  test("Unary Operator Example") {
    val code = readFile("functional/unittests/Unary.finca")
    val fun = FunctionalExecutor.loadFunction(code)
    assert(fun.execute("main", Seq(q"10")) == fun.resultVal(-10))
  }

  test("Method Call Example") {
    val code = readFile("functional/unittests/MethodCall.finca")
    val fun = FunctionalExecutor.loadFunction(code)
    assert(fun.execute("main", Seq(q""""abcdefg"""")) == fun.resultVal(true))
    assert(fun.execute("main", Seq(q""""abdefg"""")) == fun.resultVal(false))
  }

  test("Tuple Input Example") {
    val code = readFile("functional/unittests/TupleAsInput.finca")
    val fun = FunctionalExecutor.loadFunction(code)
    assert(fun.execute("main", Seq(q"10", q""""x"""")) == fun.resultVal("x"))
    assert(fun.execute("main2", Seq(q"10", q""""x"""")) == fun.results(Seq(Seq(10, "x"))))
  }

  test("Simple Set Intersection") {
    val code = readFile("functional/unittests/SetIntersection.finca")
    val fun = FunctionalExecutor.loadFunction(code)
    assert(fun.execute("main", Seq()) == fun.results(Seq(Seq(1), Seq(3))))
  }

  test("Complex Set Intersection") {
    val code = readFile("functional/unittests/ComplexSetIntersection.finca")
    val fun = FunctionalExecutor.loadFunction(code, FunctionalOptions().withOptimizations(Seq()))
    assert(fun.execute("main", Seq()) == fun.results(Seq(Seq(3), Seq(4))))
    assert(fun.execute("main2", Seq()) == fun.results(Seq(Seq(1), Seq(4))))
    assert(fun.execute("main3", Seq()) == fun.results(Seq(Seq(16))))
    assert(fun.execute("main4", Seq()) == fun.results(Seq(Seq(10))))
    assert(fun.execute("main5", Seq()) == fun.results(Seq(Seq(3), Seq(4))))
    assert(fun.execute("main6", Seq()) == fun.results(Seq(Seq(3), Seq(4), Seq(5))))
  }

  test("fixpoint function") {
    val code = readFile("functional/unittests/FixFunction.finca")
    val fun = FunctionalExecutor.loadFunction(code, FunctionalOptions().withOptimizations(Seq()))
    assert(fun.execute("main", Seq(q"1")).isEmpty)
  }
}
