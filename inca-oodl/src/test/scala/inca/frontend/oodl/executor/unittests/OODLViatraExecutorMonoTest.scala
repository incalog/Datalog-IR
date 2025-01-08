package inca.frontend.oodl.executor.unittests

import inca.frontend.oodl.compile.{CompiledOODLUnit, OODLCompilerOptions}
import inca.frontend.oodl.executor.{OODLExecutor, TypeCastException}
import inca.ir.execution.Relation
import inca.util.FileUtil
import inca.viatra.backend.Executor
import org.scalatest.funsuite.AnyFunSuite

class OODLViatraExecutorMonoTest extends AnyFunSuite:
  val options = OODLCompilerOptions.fromResource("objectoriented/Options.ini")
  val exec: OODLExecutor = new OODLExecutor(new Executor)

  // Unittests
  test("Count mono") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/mono/Count.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLUnit.pipeline)
    compiled.setOptimizationPipeline(CompiledOODLUnit.optimizationPipeline)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(2)(res.entries.head)
  }

  test("Map mono") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/mono/Map.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLUnit.pipeline)
    compiled.setOptimizationPipeline(CompiledOODLUnit.optimizationPipeline)
    // Important: Include post processing pipeline for custom mono type
    compiled.setPostProcessingPipeline(compiled.viatraPostProcessingPipeline)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq())
    assertResult("SID(Pos)")(res.entries.head.toString)
  }

  test("Map mono with nested mono.Set") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/mono/MapWithSet.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLUnit.pipeline)
    compiled.setOptimizationPipeline(CompiledOODLUnit.optimizationPipeline)
    // Important: Include post processing pipeline for custom mono type
    compiled.setPostProcessingPipeline(compiled.viatraPostProcessingPipeline)
    val loaded = exec.loadOODL(compiled)
    var res = loaded.execute("main", Seq())
    val setAdt = res.entries.head
    val query = Relation.from("Set$TInt$enum", Seq("$set"), Seq(Seq(setAdt)))
    res = loaded.engine.read(query).project(1, 2)
    assertResult(Set(1, 2))(res.toSet)
  }

  test("User mono") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/mono/User.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLUnit.pipeline)
    compiled.setOptimizationPipeline(CompiledOODLUnit.optimizationPipeline)
    // Important: Include the post processing pipeline to make sure the whole module is translated to scala as well
    compiled.setPostProcessingPipeline(compiled.viatraPostProcessingPipeline)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq())
    assertResult("SID(Pos)")(res.entries.head.toString)
  }