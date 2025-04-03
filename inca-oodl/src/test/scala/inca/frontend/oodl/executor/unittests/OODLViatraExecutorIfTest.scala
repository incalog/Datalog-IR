package inca.frontend.oodl.executor.unittests

import inca.frontend.oodl.compile.{CompiledOODLUnit, OODLCompilerOptions}
import inca.frontend.oodl.executor.{OODLExecutor, TypeCastException}
import inca.ir.execution.Relation
import inca.util.FileUtil
import inca.viatra.backend.Executor
import org.scalatest.funsuite.AnyFunSuite

class OODLViatraExecutorIfTest extends AnyFunSuite:
  val options = OODLCompilerOptions.fromResource("objectoriented/Options.ini")
  val exec: OODLExecutor = new OODLExecutor(new Executor)

  /** If */

  test("If") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/if/If.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLUnit.pipeline)
    compiled.setOptimizationPipeline(compiled.optimizationPipeline)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq(1, 1))
    assertResult(11)(res.entries.head)
  }

  test("If Duplicate") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/if/IfDuplicate.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLUnit.pipeline)
    compiled.setOptimizationPipeline(compiled.optimizationPipeline)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq(1, 1))
    assertResult(10)(res.entries.head)
  }

  test("If False") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/if/IfFalse.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLUnit.pipeline)
    compiled.setOptimizationPipeline(compiled.optimizationPipeline)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(1)(res.entries.head)
  }

  test("If True") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/if/IfTrue.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLUnit.pipeline)
    compiled.setOptimizationPipeline(compiled.optimizationPipeline)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(1)(res.entries.head)
  }

  test("If Object") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/if/IfObject.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLUnit.pipeline)
    compiled.setOptimizationPipeline(compiled.optimizationPipeline)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq(1))
    assertResult(1)(res.entries.head)
  }
