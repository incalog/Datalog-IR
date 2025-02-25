package inca.frontend.functional.executor.typechecker

import inca.ddlog.backend.Executor
import inca.frontend.functional.compile.{CompiledFunctionalUnit, FunctionalCompilerOptions}
import inca.frontend.functional.executor.FunctionalExecutor
import inca.util.FileUtil
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.Ignore

@Ignore
class FunctionalDDLogExecutorTest extends AnyFunSuite:
  val options = FunctionalCompilerOptions.fromResource("functional/Options.ini")
  val exec: FunctionalExecutor = new FunctionalExecutor(new Executor())

  test("TypeChecker") {
    val code = FileUtil.readFileFromResource("functional/typechecker/TypeChecker.finca")
    val compiled = exec.compileFunction(code, options)
    compiled.setPipeline(CompiledFunctionalUnit.pipeline)
    compiled.setOptimizationPipeline(CompiledFunctionalUnit.optimizationPipeline)
    val loaded = exec.loadFunction(compiled)

    val res = loaded.execute("main", Seq(prog1))
    assertResult(
      "Some_Type(TFun(TFun(TFun(TInt(),TInt()),TFun(TInt(),TInt())),TFun(TFun(TFun(TInt(),TInt()),TFun(TInt(),TInt())),TFun(TFun(TInt(),TInt()),TFun(TInt(),TInt())))))"
    )(
      res.entries.head.toString
    )
  }