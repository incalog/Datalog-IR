package inca.frontend.functional.executor.higherorder

import inca.frontend.functional.executor.FunctionalExecutor
import inca.util.FileUtil
import org.scalatest.funsuite.AnyFunSuite

class FunctionalExecutorTest extends AnyFunSuite:
  val exec: FunctionalExecutor = new FunctionalExecutor(inca.viatra.Executor)

  test("Apply") {
    val code = FileUtil.readFile("functional/higherorder/Apply.finca")
    val compiled = exec.compileFunction(code)
    val loaded = exec.loadFunction(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(6)(res.entries.head)
  }

  test("Compose Fun") {
    val code = FileUtil.readFile("functional/higherorder/ComposeFun.finca")
    val compiled = exec.compileFunction(code)
    val loaded = exec.loadFunction(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(16)(res.entries.head)
  }

  test("Compose Lambda") {
    val code = FileUtil.readFile("functional/higherorder/ComposeLambda.finca")
    val compiled = exec.compileFunction(code)
    val loaded = exec.loadFunction(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(16)(res.entries.head)
  }

  test("Lambda") {
    val code = FileUtil.readFile("functional/higherorder/Lambda.finca")
    val compiled = exec.compileFunction(code)
    val loaded = exec.loadFunction(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(21)(res.entries.head)
  }

  test("Lambda Higher order") {
    val code = FileUtil.readFile("functional/higherorder/LambdaHigherOrder.finca")
    val compiled = exec.compileFunction(code)
    val loaded = exec.loadFunction(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(63)(res.entries.head)
  }

  test("Transitive") {
    val code = FileUtil.readFile("functional/higherorder/Transitive.finca")
    val compiled = exec.compileFunction(code)
    val loaded = exec.loadFunction(compiled)
    var res = loaded.execute("foo", Seq())
    assertResult(Set((2, 2), (2, 1), (1, 2), (1, 1), (3, 2), (3, 1), (3, 3), (2, 3), (1, 3)))(res.toSet)
    res = loaded.execute("bar", Seq())
    assertResult(Set((2, 2), (4, 1), (1, 2), (1, 4), (1, 1), (2, 4), (3, 2), (3, 1), (3, 3), (4, 3), (2, 3), (1, 3), (3, 4), (2, 1), (4, 2), (4, 4)))(res.toSet)
  }