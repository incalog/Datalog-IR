package inca.frontend.functional.executor

import inca.frontend.functional.compile.{CompiledFunctionalModule, GenerateDatalog}
import inca.frontend.functional.executor.FunctionalExecutor
import org.scalatest.funsuite.AnyFunSuite

import inca.frontend.functional.syntax.*
import inca.util.FileUtil

class ExecutorTest extends AnyFunSuite {
  test("Base 1") {
    val code = FileUtil.readFile("functional/unittests/Base1.finca")
    val compiled = FunctionalExecutor.compileFunction(code)
    val loaded = FunctionalExecutor.loadFunction(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(43)(res.entries.head)
  }

  test("Fac") {
    val code = FileUtil.readFile("functional/unittests/Fact.finca")
    val compiled = FunctionalExecutor.compileFunction(code)
    val loaded = FunctionalExecutor.loadFunction(compiled)
    val res = loaded.execute("main", Seq(5))
    assertResult(120)(res.entries.head)
  }

  test("Fib") {
    val code = FileUtil.readFile("functional/unittests/Fib.finca")
    val compiled = FunctionalExecutor.compileFunction(code)
    val loaded = FunctionalExecutor.loadFunction(compiled)
    val res = loaded.execute("main", Seq(7))
    assertResult(13)(res.entries.head)
  }
}
