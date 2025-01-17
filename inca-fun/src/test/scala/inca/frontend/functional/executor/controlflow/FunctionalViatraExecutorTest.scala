package inca.frontend.functional.executor.controlflow

import inca.frontend.functional.compile.{CompiledFunctionalUnit, FunctionalCompilerOptions}
import inca.frontend.functional.executor.FunctionalExecutor
import inca.ir.execution.{Relation, UnitRelation}
import inca.util.FileUtil
import inca.viatra.backend.Executor
import org.scalatest.funsuite.AnyFunSuite


class FunctionalViatraExecutorTest extends AnyFunSuite:
  val options = FunctionalCompilerOptions.fromResource("functional/Options.ini")
  val exec: FunctionalExecutor = new FunctionalExecutor(new Executor)

  test("Interval Analysis") {
    val code = FileUtil.readFileFromResource("functional/controlflow/Interval.finca")
    val compiled = exec.compileFunction(code, options)
    compiled.setPipeline(CompiledFunctionalUnit.pipeline)
    compiled.setPostProcessingPipeline(CompiledFunctionalUnit.viatraPostProcessingPipeline)
    compiled.setOptimizationPipeline(CompiledFunctionalUnit.optimizationPipeline)
    val loaded = exec.loadFunction(compiled)
    val setAdt = loaded.execute("mainFinalVar", Seq(prog1)).entries.head
    val query = Relation.from("Set$$TString_Val$$enum", Seq("$set", "$elem$0", "$elem$1"), Seq(Seq(setAdt, null, null)))
    val res = loaded.engine.read(query).project(1)
    assertResult(
      Seq("(x,IntervalVal(TopInterval()))", "(y,IntervalVal(TopInterval()))")
    )(
      res.entries.map(_.toString)
    )
  }