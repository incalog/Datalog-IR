package inca.frontend.functional.executor.controlflow

import inca.frontend.functional.compile.{CompiledFunctionalUnit, FunctionalCompilerOptions}
import inca.frontend.functional.executor.FunctionalExecutor
import inca.ir.execution.Relation
import inca.ascent.backend.Executor
import inca.util.FileUtil
import org.scalatest.funsuite.AnyFunSuite


class FunctionalAscentExecutorTest extends AnyFunSuite:
  val options = FunctionalCompilerOptions.fromResource("functional/Options.ini")
  val exec: FunctionalExecutor = new FunctionalExecutor(new Executor)

  test("Control Flow Analysis") {
    val code = FileUtil.readFileFromResource("functional/controlflow/CFlow.finca")
    val compiled = exec.compileFunction(code, options)
    compiled.setPipeline(CompiledFunctionalUnit.pipeline)
    compiled.setOptimizationPipeline(CompiledFunctionalUnit.fastOptimizationPipeline)
    val loaded = exec.loadFunction(compiled)
    val setAdt = loaded.execute("mainTransitiveFlow", Seq(prog1)).entries.head
    val query = Relation.from("Set__Stm_Stm__enum", Seq("$set", "$elem$0", "$elem$1"), Seq(Seq(setAdt, null, null)))
    val res = loaded.engine.read(query).project(1)
    assertResult(25)(res.entries.size)
  }