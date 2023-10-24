package inca.frontend.functional.executor.unittests

import inca.frontend.functional.executor.FunctionalExecutor
import inca.util.FileUtil
import org.scalatest.funsuite.AnyFunSuite

class FunctionalExecutorTest extends AnyFunSuite {
  val exec: FunctionalExecutor = new FunctionalExecutor(inca.viatra.Executor)

  test("Base 1") {
    val code = FileUtil.readFile("functional/unittests/Base1.finca")
    val compiled = exec.compileFunction(code)
    val loaded = exec.loadFunction(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(43)(res.entries.head)
  }

  test("Fac") {
    val code = FileUtil.readFile("functional/unittests/Fact.finca")
    val compiled = exec.compileFunction(code)
    val loaded = exec.loadFunction(compiled)
    val res = loaded.execute("main", Seq(5))
    assertResult(120)(res.entries.head)
  }

  test("Fib") {
    val code = FileUtil.readFile("functional/unittests/Fib.finca")
    val compiled = exec.compileFunction(code)
    val loaded = exec.loadFunction(compiled)
    val res = loaded.execute("main", Seq(7))
    assertResult(13)(res.entries.head)
  }

  test("Inc") {
    val code = FileUtil.readFile("functional/unittests/Inc.finca")
    val compiled = exec.compileFunction(code)
    val loaded = exec.loadFunction(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(1)(res.entries.head)
  }

  test("Unary") {
    val code = FileUtil.readFile("functional/unittests/Unary.finca")
    val compiled = exec.compileFunction(code)
    val loaded = exec.loadFunction(compiled)
    val res = loaded.execute("main", Seq(5))
    assertResult(-5)(res.entries.head)
  }

  test("Var") {
    val code = FileUtil.readFile("functional/unittests/Var.finca")
    val compiled = exec.compileFunction(code)
    val loaded = exec.loadFunction(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(43)(res.entries.head)
  }

  test("If") {
    val code = FileUtil.readFile("functional/unittests/If.finca")
    val compiled = exec.compileFunction(code)
    val loaded = exec.loadFunction(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(7)(res.entries.head)
  }

  test("If2") {
    val code = FileUtil.readFile("functional/unittests/If2.finca")
    val compiled = exec.compileFunction(code)
    val loaded = exec.loadFunction(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(10)(res.entries.head)
  }

  // TODO: Currently we are not getting back booleans, but ints because of the lowering
  //  Either we want an unlower or we want to support booleans ?
  test("Parametric Eq") {
    val code = FileUtil.readFile("functional/unittests/ParametricEq.finca")
    val compiled = exec.compileFunction(code)
    var loaded = exec.loadFunction(compiled)
    var res = loaded.execute("main", Seq(1, 1))
    assertResult(1)(res.entries.head)

    // clear the input
    loaded = exec.loadFunction(compiled)
    res = loaded.execute("main", Seq(1, 2))
    assertResult(0)(res.entries.head)
  }

  test("Parametric function") {
    val code = FileUtil.readFile("functional/unittests/ParametricFunction.finca")
    val compiled = exec.compileFunction(code)
    val loaded = exec.loadFunction(compiled)
    val res = loaded.execute("main", Seq())
    assertResult((12, 1))(res.entries.head)
  }

  test("Tuple as input") {
    val code = FileUtil.readFile("functional/unittests/TupleAsInput.finca")
    val compiled = exec.compileFunction(code)
    var loaded = exec.loadFunction(compiled)
    var res = loaded.execute("main", Seq(1, "A"))
    assertResult("A")(res.entries.head)

    loaded = exec.loadFunction(compiled)
    res = loaded.execute("main2", Seq(1, "A"))
    assertResult((1, "A"))(res.entries.head)
  }

  // With our current design main must not be recursive
  /*test("Fix function") {
    val code = FileUtil.readFile("functional/unittests/FixFunction.finca")
    val compiled = exec.compileFunction(code)
    val loaded = exec.loadFunction(compiled)
    val res = loaded.execute("main", Seq(1))
    assertResult(13)(res.entries.head)
  }*/

  test("Plus") {
    val code = FileUtil.readFile("functional/unittests/Plus.finca")
    val compiled = exec.compileFunction(code)
    val loaded = exec.loadFunction(compiled)
    val res = loaded.execute("main", Seq())
    assertResult("Succ(Succ(Succ(Succ(Succ(Zero())))))")(res.entries.head.toString)
  }

  test("Complex set intersection") {
    val code = FileUtil.readFile("functional/unittests/ComplexSetIntersection.finca")
    val compiled = exec.compileFunction(code)
    val loaded = exec.loadFunction(compiled)
    val res = loaded.execute("main", Seq())
    println(res)
    //assertResult("Succ(Succ(Succ(Succ(Succ(Zero())))))")(res.entries.head.toString)
  }
}
