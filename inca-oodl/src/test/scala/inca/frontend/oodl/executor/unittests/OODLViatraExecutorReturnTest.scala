package inca.frontend.oodl.executor.unittests

import inca.frontend.oodl.compile.{CompiledOODLModule, OODLCompilerOptions}
import inca.frontend.oodl.executor.{OODLExecutor, TypeCastException}
import inca.ir.execution.Relation
import inca.util.FileUtil
import org.scalatest.funsuite.AnyFunSuite

class OODLViatraExecutorReturnTest extends AnyFunSuite:
  val options = OODLCompilerOptions.fromResource("objectoriented/Options.ini")
  val exec: OODLExecutor = new OODLExecutor(inca.viatra.Executor)

  test("Return with cond true") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/return/Return.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq(1))
    assertResult(1)(res.entries.head)
  }

  test("Return with cond false") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/return/Return.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq(0))
    assertResult(2)(res.entries.head)
  }

  test("Return implicit") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/return/ReturnImplicit.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(1)(res.entries.head)
  }

  test("Return implicit unit") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/return/ReturnImplicitUnit.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(Set())(res.entries.toSet)
  }

  test("Return twice") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/return/ReturnTwice.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(1)(res.entries.head)
  }

