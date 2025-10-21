package inca.frontend.functional.executor.lambdacalculus

import inca.frontend.functional.compile.{CompiledFunctionalUnit, FunctionalCompilerOptions}
import inca.frontend.functional.executor.FunctionalExecutor
import inca.ir.execution.{Relation, UnitRelation}
import inca.util.FileUtil
import inca.viatra.backend.Executor
import org.scalatest.funsuite.AnyFunSuite


class FunctionalViatraExecutorTest extends AnyFunSuite:
  val options = FunctionalCompilerOptions.fromResource("functional/Options.ini")
  val exec: FunctionalExecutor = new FunctionalExecutor(new Executor)

  test("Lambda Calculus - Interpreter") {
    val code = FileUtil.readFileFromResource("functional/lambdacalculus/LambdaCalculus.finca")
    val compiled = exec.compileFunction(code, options)
    compiled.setPipeline(CompiledFunctionalUnit.pipeline)
    compiled.setOptimizationPipeline(CompiledFunctionalUnit.optimizationPipeline)
    val loaded = exec.loadFunction(compiled)
    val res = loaded.execute("mainInterp", Seq(interpProg1))
    assertResult(
      "SomeVal(VClosure(x,Var(y),BindEnv(y,VNum(1),EmptyEnv())))"
    )(
      res.entries.head.toString
    )
  }

  test("Lambda Calculus - Type checker") {
    val code = FileUtil.readFileFromResource("functional/lambdacalculus/LambdaCalculus.finca")
    val compiled = exec.compileFunction(code, options)
    compiled.setPipeline(CompiledFunctionalUnit.pipeline)
    compiled.setOptimizationPipeline(CompiledFunctionalUnit.optimizationPipeline)
    val loaded = exec.loadFunction(compiled)
    val res = loaded.execute("mainTypeOf", Seq(typeProg1))
    assertResult(
      "SomeType(TInt())"
    )(
      res.entries.head.toString
    )
  }

  test("Lambda Calculus - Type erasure") {
    val code = FileUtil.readFileFromResource("functional/lambdacalculus/LambdaCalculus.finca")
    val compiled = exec.compileFunction(code, options)
    compiled.setPipeline(CompiledFunctionalUnit.pipeline)
    compiled.setOptimizationPipeline(CompiledFunctionalUnit.optimizationPipeline)
    val loaded = exec.loadFunction(compiled)
    val res = loaded.execute("mainErase", Seq(typeProg1))
    assertResult(
      "App(Lam(x,Var(x)),Num(1337))"
    )(
      res.entries.head.toString
    )
  }

  test("Lambda Calculus - Main") {
    //Executor.initializeLogging()
    //Executor.enableDebugLogging()

    val code = FileUtil.readFileFromResource("functional/lambdacalculus/LambdaCalculus.finca")
    val compiled = exec.compileFunction(code, options)
    compiled.setPipeline(CompiledFunctionalUnit.pipeline)
    compiled.setOptimizationPipeline(CompiledFunctionalUnit.optimizationPipeline)

    val prog = generateTypedProg(10)
    //val prog = generateTypedProg(50)

    val loaded = exec.loadFunction(compiled)
    val res = loaded.execute("main", Seq(prog))
    assertResult(
      "SomeVal(VNum(1337))"
    )(
      res.entries.head.toString
    )
  }