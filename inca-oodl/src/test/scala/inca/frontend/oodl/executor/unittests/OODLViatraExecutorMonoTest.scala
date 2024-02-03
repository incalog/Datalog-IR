package inca.frontend.oodl.executor.unittests

import inca.frontend.oodl.compile.{CompiledOODLModule, OODLCompilerOptions}
import inca.frontend.oodl.executor.{OODLExecutor, TypeCastException}
import inca.ir.execution.Relation
import inca.util.FileUtil
import org.scalatest.funsuite.AnyFunSuite

class OODLViatraExecutorMonoTest extends AnyFunSuite:
  val options = OODLCompilerOptions.fromResource("objectoriented/Options.ini")
  val exec: OODLExecutor = new OODLExecutor(new inca.viatra.Executor)

  // Unittests
  test("Count mono") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/mono/Count.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(2)(res.entries.head)
  }

  test("Map mono") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/mono/Map.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    // Important: Include post processing pipeline for custom mono type
    compiled.setPostProcessingPipeline(compiled.viatraPostProcessingPipeline)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq())
    assertResult("SID(Pos)")(res.entries.head.toString)
  }

  test("User mono") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/mono/User.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    // Important: Include the post processing pipeline to make sure the whole module is translated to scala as well
    compiled.setPostProcessingPipeline(compiled.viatraPostProcessingPipeline)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq())
    assertResult("SID(Pos)")(res.entries.head.toString)
  }