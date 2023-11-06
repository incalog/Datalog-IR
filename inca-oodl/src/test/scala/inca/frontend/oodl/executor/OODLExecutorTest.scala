package inca.frontend.oodl.executor

import inca.util.FileUtil
import org.scalatest.funsuite.AnyFunSuite

class OODLExecutorTest extends AnyFunSuite:
  val exec: OODLExecutor = new OODLExecutor(inca.viatra.Executor)

  // Unittests

  test("Base") {
    val code = FileUtil.readFile("objectoriented/unittests/Base.oodl")
    val compiled = exec.compileOODL(code)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(43)(res.entries.head)
  }

  test("Factorial") {
    val code = FileUtil.readFile("objectoriented/unittests/Fact.oodl")
    val compiled = exec.compileOODL(code)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq(5))
    assertResult(120)(res.entries.head)
  }

  test("Fibonacci") {
    val code = FileUtil.readFile("objectoriented/unittests/Fib.oodl")
    val compiled = exec.compileOODL(code)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq(10))
    assertResult(55)(res.entries.head)
  }

  // We need more optimizations to execute the full program
  //  + Disjunction lowering is way to slow on this (is there an endless loop?)
  test("InstanceOf") {
    val code = FileUtil.readFile("objectoriented/unittests/InstanceOf.oodl")
    val compiled = exec.compileOODL(code)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(1)(res.entries.head)
  }

  test("Dynamic Dispatch") {
    val code = FileUtil.readFile("objectoriented/unittests/DynamicDispatch.oodl")
    val compiled = exec.compileOODL(code)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq())
    assertResult("BBC")(res.entries.head)
  }

  // TODO: Currently not supported, need more optimizations (how should equality on objects beeing handled ?)
  test("Equals") {
    val code = FileUtil.readFile("objectoriented/unittests/Equals.oodl")
    val compiled = exec.compileOODL(code)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(1)(res.entries.head)
  }
