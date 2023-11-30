package inca.frontend.oodl.executor.unittests

import inca.frontend.oodl.compile.CompiledOODLModule
import inca.frontend.oodl.executor.{OODLExecutor, TypeCastException}
import inca.ir.execution.Relation
import inca.util.FileUtil
import org.scalatest.funsuite.AnyFunSuite

class OODLViatraExecutorIfTest extends AnyFunSuite:
  val exec: OODLExecutor = new OODLExecutor(inca.viatra.Executor)

  /** If */

  test("If") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/if/If.oodl")
    val compiled = exec.compileOODL(code)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq(1, 1))
    assertResult(11)(res.entries.head)
  }

  test("If Duplicate") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/if/IfDuplicate.oodl")
    val compiled = exec.compileOODL(code)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq(1, 1))
    assertResult(10)(res.entries.head)
  }

  test("If False") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/if/IfFalse.oodl")
    val compiled = exec.compileOODL(code)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(1)(res.entries.head)
  }

  test("If True") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/if/IfTrue.oodl")
    val compiled = exec.compileOODL(code)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(1)(res.entries.head)
  }
