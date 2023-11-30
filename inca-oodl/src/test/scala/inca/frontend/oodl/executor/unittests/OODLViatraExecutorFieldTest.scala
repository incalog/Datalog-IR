package inca.frontend.oodl.executor.unittests

import inca.frontend.oodl.compile.CompiledOODLModule
import inca.frontend.oodl.executor.{OODLExecutor, TypeCastException}
import inca.ir.execution.Relation
import inca.util.FileUtil
import org.scalatest.funsuite.AnyFunSuite

class OODLViatraExecutorFieldTest extends AnyFunSuite:
  val exec: OODLExecutor = new OODLExecutor(inca.viatra.Executor)

  /** Case class */

  test("Field access") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/field/FieldAccess.oodl")
    val compiled = exec.compileOODL(code)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq(16, 2))
    assertResult(8)(res.entries.head)
  }

  test("Field access nested") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/field/FieldAccessNested.oodl")
    val compiled = exec.compileOODL(code)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(3)(res.entries.head)
  }

  test("Field inheritance") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/field/FieldInheritance.oodl")
    val compiled = exec.compileOODL(code)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(10)(res.entries.head)
  }
