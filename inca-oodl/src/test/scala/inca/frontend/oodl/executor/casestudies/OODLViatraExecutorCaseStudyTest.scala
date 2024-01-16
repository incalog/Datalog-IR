package inca.frontend.oodl.executor.casestudies

import inca.frontend.oodl.compile.{CompiledOODLModule, OODLCompilerOptions}
import inca.frontend.oodl.executor.{OODLExecutor, TypeCastException}
import inca.ir.execution.Relation
import inca.util.FileUtil
import org.scalatest.funsuite.AnyFunSuite

class OODLViatraExecutorCaseStudyTest extends AnyFunSuite:
  val options = OODLCompilerOptions.fromResource("objectoriented/Options.ini")
  val exec: OODLExecutor = new OODLExecutor(new inca.viatra.Executor)

  test("DependencyAnalysis") {
    val endNode = 50
    val step = 10

    val code = FileUtil.readFileFromResource("objectoriented/casestudies/DependencyAnalysis.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)

    val start = System.nanoTime()
    var res = loaded.execute("main", Seq(endNode, step))
    val diff = System.nanoTime() - start

    val setAdt = res.entries.head
    val query = Relation.from("Set$TString$enum", Seq("$set"), Seq(Seq(setAdt)))
    res = loaded.engine.read(query).project(1, 2)

    println(diff.toDouble / 1000 / 1000 / 1000)
    assertResult(0.to(endNode).map("a" + _).toSet)(res.toSet)
  }