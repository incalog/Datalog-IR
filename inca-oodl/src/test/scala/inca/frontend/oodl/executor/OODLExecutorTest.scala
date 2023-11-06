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

  test("Dynamic Dispatch") {
    val code = FileUtil.readFile("objectoriented/unittests/DynamicDispatch.oodl")
    val compiled = exec.compileOODL(code)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq())
    assertResult("BBC")(res.entries.head)
  }
