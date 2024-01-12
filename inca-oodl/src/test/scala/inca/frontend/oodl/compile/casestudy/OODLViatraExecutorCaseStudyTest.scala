package inca.frontend.oodl.compile.casestudy

import inca.frontend.oodl.compile.{CompiledOODLModule, OODLCompilerOptions}
import inca.frontend.oodl.executor.{OODLExecutor, TypeCastException}
import inca.ir.execution.Relation
import inca.util.FileUtil
import org.scalatest.Ignore
import org.scalatest.funsuite.AnyFunSuite

// TODO: Why is this so, so much slower than the old hacked mono implementation ?
class OODLViatraExecutorCaseStudyTest extends AnyFunSuite:
  val options = OODLCompilerOptions.fromResource("objectoriented/Options.ini")
  val exec: OODLExecutor = new OODLExecutor(new inca.viatra.Executor)

  test("DependencyAnalysis") {
    val code = FileUtil.readFileFromResource("objectoriented/casestudies/DependencyAnalysis.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    var res = loaded.execute("main", Seq(10, 2))
    val setAdt = res.entries.head
    val query = Relation.from("Set$TString$enum", Seq("$set"), Seq(Seq(setAdt)))
    res = loaded.engine.read(query).project(1, 2)
    // TODO: Is this even the correct expected result ?
    assertResult(Set("a0", "a1", "a2"))(res.toSet)
  }