package inca.frontend.oodl.executor.unittests

import inca.frontend.oodl.compile.{CompiledOODLModule, GenerateScala, OODLCompilerOptions}
import inca.frontend.oodl.executor.{OODLExecutor, TypeCastException}
import inca.ir.execution.Relation
import inca.util.FileUtil
import inca.viatra.backend.Executor
import org.scalatest.funsuite.AnyFunSuite

class OODLViatraExecutorSetFoldTest extends AnyFunSuite:
  val options = OODLCompilerOptions.fromResource("objectoriented/Options.ini")
  val exec: OODLExecutor = new OODLExecutor(new Executor)

  test("Set fold int") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/setfold/PrimitiveSetFold.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    compiled.setPostProcessingPipeline(compiled.viatraPostProcessingPipeline)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(6)(res.entries.head)
  }

  test("Set fold case class") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/setfold/CaseClassSetFold.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    compiled.setPostProcessingPipeline(compiled.viatraPostProcessingPipeline)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(6)(res.entries.head)
  }
