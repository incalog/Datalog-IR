package inca.frontend.functional.executor.explang

import inca.frontend.functional.compile.{CompiledFunctionalUnit, FunctionalCompilerOptions}
import inca.frontend.functional.executor.FunctionalExecutor
import inca.ir.execution.Relation
import inca.util.FileUtil
import inca.viatra.backend.Executor
import org.scalatest.funsuite.AnyFunSuite


class FunctionalViatraExecutorTest extends AnyFunSuite:
  val options = FunctionalCompilerOptions.fromResource("functional/Options.ini")
  val exec: FunctionalExecutor = new FunctionalExecutor(new Executor)

  test("Desugar") {
    val code = FileUtil.readFileFromResource("functional/explang/Desugar.finca")
    val compiled = exec.compileFunction(code, options)
    compiled.setPipeline(CompiledFunctionalUnit.pipeline)
    compiled.setOptimizationPipeline(CompiledFunctionalUnit.optimizationPipeline)

    val loaded = exec.loadFunction(compiled)
    loaded.execute("main", Seq(prog1))
    loaded.engine.readAll().foreach(r => println(r.asTable))
  }