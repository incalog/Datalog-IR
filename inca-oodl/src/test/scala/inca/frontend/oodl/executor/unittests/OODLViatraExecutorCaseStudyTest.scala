package inca.frontend.oodl.executor.unittests

import inca.frontend.oodl.compile.{CompiledOODLModule, OODLCompilerOptions}
import inca.frontend.oodl.executor.{OODLExecutor, TypeCastException}
import inca.ir.execution.Relation
import inca.util.FileUtil
import org.scalatest.Ignore
import org.scalatest.funsuite.AnyFunSuite

// Compiles but doesn't terminate
@Ignore
class OODLViatraExecutorCaseStudyTest extends AnyFunSuite:
  val options = OODLCompilerOptions.fromResource("objectoriented/Options.ini")
  val exec: OODLExecutor = new OODLExecutor(inca.viatra.Executor)

  test("DependencyAnalysis") {
    val code = FileUtil.readFileFromResource("objectoriented/casestudies/DependencyAnalysis.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq(10, 2))
    assertResult(11)(res.entries.head)
  }