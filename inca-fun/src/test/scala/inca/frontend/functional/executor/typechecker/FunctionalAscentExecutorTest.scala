package inca.frontend.functional.executor.typechecker

import inca.frontend.functional.compile.{CompiledFunctionalUnit, FunctionalCompilerOptions}
import inca.frontend.functional.executor.FunctionalExecutor
import inca.ir.execution.{Relation, UnitRelation}
import inca.util.FileUtil
import inca.ascent.backend.Executor
import inca.ir.execution.ThreadCount.Fixed
import org.scalatest.funsuite.AnyFunSuite


class FunctionalAscentExecutorTest extends AnyFunSuite:
  val options = FunctionalCompilerOptions.fromResource("functional/Options.ini")
  val exec: FunctionalExecutor = new FunctionalExecutor(new Executor(Fixed(1)))

  test("TypeChecker") {
    val code = FileUtil.readFileFromResource("functional/itypes/TypeChecker.finca")
    val compiled = exec.compileFunction(code, options)
    compiled.setPipeline(CompiledFunctionalUnit.pipeline)
    compiled.setOptimizationPipeline(CompiledFunctionalUnit.optimizationPipeline)
    val loaded = exec.loadFunction(compiled)
    //val diff = loaded.engine.measure(UnitRelation("main"))
    //println(diff)
    //println(loaded.engine.readAll().map(_.size).sum)
    val res = loaded.execute("main", Seq())
    assertResult(
      "Some_Type(TFun(TFun(TFun(TInt, TInt), TFun(TInt, TInt)), TFun(TFun(TFun(TInt, TInt), TFun(TInt, TInt)), TFun(TFun(TInt, TInt), TFun(TInt, TInt)))))"
    )(
      res.entries.head.toString
    )
  }