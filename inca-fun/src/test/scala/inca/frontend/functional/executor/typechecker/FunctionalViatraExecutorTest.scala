package inca.frontend.functional.executor.typechecker

import inca.frontend.functional.compile.{CompiledFunctionalUnit, FunctionalCompilerOptions}
import inca.frontend.functional.executor.FunctionalExecutor
import inca.util.FileUtil
import inca.ir.execution.{Relation, UnitRelation}
import inca.viatra.backend.Executor
import org.scalatest.funsuite.AnyFunSuite


class FunctionalViatraExecutorTest extends AnyFunSuite:
  val options = FunctionalCompilerOptions.fromResource("functional/Options.ini")
  val exec: FunctionalExecutor = new FunctionalExecutor(new Executor)

  test("TypeChecker") {
    val code = FileUtil.readFileFromResource("functional/itypes/TypeChecker.finca")
    val compiled = exec.compileFunction(code, options)
    compiled.setPipeline(CompiledFunctionalUnit.pipeline)
    compiled.setOptimizationPipeline(CompiledFunctionalUnit.optimizationPipeline)
    val loaded = exec.loadFunction(compiled)
    //println(loaded.engine.measure(UnitRelation("main")))
    //println(loaded.engine.readAll().map(_.size).sum)
    val res = loaded.execute("main", Seq())
    assertResult(
      "Some$Type(TFun(TFun(TFun(TInt(),TInt()),TFun(TInt(),TInt())),TFun(TFun(TFun(TInt(),TInt()),TFun(TInt(),TInt())),TFun(TFun(TInt(),TInt()),TFun(TInt(),TInt())))))"
    )(
      res.entries.head.toString
    )
  }