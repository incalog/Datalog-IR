package inca.frontend.functional.executor.branching

import inca.frontend.functional.compile.{CompiledFunctionalModule, FunctionalCompilerOptions}
import inca.frontend.functional.executor.FunctionalExecutor
import inca.ir.execution.{Relation, Relation2, UnitRelation}
import inca.util.FileUtil
import inca.viatra.backend.Executor
import org.scalatest.funsuite.AnyFunSuite


class FunctionalViatraExecutorTest extends AnyFunSuite:
  val options = FunctionalCompilerOptions.fromResource("functional/Options.ini")
  val exec: FunctionalExecutor = new FunctionalExecutor(new Executor)

  test("Branching") {
    val code = FileUtil.readFileFromResource("functional/branching/branching.finca")
    val compiled = exec.compileFunction(code, options)
    compiled.setPipeline(CompiledFunctionalModule.pipeline)
    val loaded = exec.loadFunction(compiled)
    //println(loaded.engine.measure(UnitRelation("main")))
    //println(loaded.engine.readAll().map(_.size).sum)
    val res = loaded.execute("main", Seq())
    assertResult(
      Seq(1)
    )(
      res.entries
    )
  }

  test("Branching 2") {
    val code = FileUtil.readFileFromResource("functional/branching/branching2.finca")
    val compiled = exec.compileFunction(code, options)
    compiled.setPipeline(CompiledFunctionalModule.pipeline)
    val loaded = exec.loadFunction(compiled)
    //println(loaded.engine.measure(UnitRelation("main")))
    //println(loaded.engine.readAll().map(_.size).sum)
    val res = loaded.execute("main", Seq())
    assertResult(
      Seq(1)
    )(
      res.entries
    )
  }