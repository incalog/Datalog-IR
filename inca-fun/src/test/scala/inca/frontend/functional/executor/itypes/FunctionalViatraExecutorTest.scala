package inca.frontend.functional.executor.itypes

import inca.frontend.functional.compile.{CompiledFunctionalModule, FunctionalCompilerOptions}
import inca.frontend.functional.executor.FunctionalExecutor
import inca.util.FileUtil
import inca.ir.execution.Relation
import inca.viatra.backend.Executor
import org.scalatest.funsuite.AnyFunSuite


class FunctionalViatraExecutorTest extends AnyFunSuite:
  val options = FunctionalCompilerOptions.fromResource("functional/Options.ini")
  val exec: FunctionalExecutor = new FunctionalExecutor(new Executor)

  test("TypeChecker") {
    val code = FileUtil.readFileFromResource("functional/itypes/TypeChecker.finca")
    val compiled = exec.compileFunction(code, options)
    compiled.setPipeline(CompiledFunctionalModule.pipeline)
    val loaded = exec.loadFunction(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(
      "Some$Type(TFun(TFun(TFun(TInt(),TInt()),TFun(TInt(),TInt())),TFun(TFun(TFun(TInt(),TInt()),TFun(TInt(),TInt())),TFun(TFun(TInt(),TInt()),TFun(TInt(),TInt())))))"
    )(
      res.entries.head.toString
    )
  }