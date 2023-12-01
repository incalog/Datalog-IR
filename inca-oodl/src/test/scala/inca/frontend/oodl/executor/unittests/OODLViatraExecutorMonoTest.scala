package inca.frontend.oodl.executor.unittests

import inca.frontend.oodl.compile.CompiledOODLModule
import inca.frontend.oodl.executor.{OODLExecutor, TypeCastException}
import inca.ir.execution.Relation
import inca.util.FileUtil
import org.scalatest.funsuite.AnyFunSuite

class OODLViatraExecutorMonoTest extends AnyFunSuite:
  val exec: OODLExecutor = new OODLExecutor(inca.viatra.Executor)

  // Unittests
  test("Count mono") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/mono/Count.oodl")
    val compiled = exec.compileOODL(code)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(2)(res.entries.head)
  }
