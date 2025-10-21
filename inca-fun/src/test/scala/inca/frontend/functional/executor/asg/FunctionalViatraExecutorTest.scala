package inca.frontend.functional.executor.asg

import inca.frontend.functional.compile.{CompiledFunctionalUnit, FunctionalCompilerOptions}
import inca.frontend.functional.executor.FunctionalExecutor
import inca.ir.execution.Relation
import inca.util.FileUtil
import inca.viatra.backend.Executor
import org.scalatest.funsuite.AnyFunSuite


class FunctionalViatraExecutorTest extends AnyFunSuite:
  val options = FunctionalCompilerOptions.fromResource("functional/Options.ini")
  val exec: FunctionalExecutor = new FunctionalExecutor(new Executor)

  test("Dependency Analysis") {
    val code = FileUtil.readFileFromResource("functional/asg/DependencyAnalysis.finca")
    val compiled = exec.compileFunction(code, options)
    compiled.setPipeline(CompiledFunctionalUnit.pipeline)
    compiled.setOptimizationPipeline(CompiledFunctionalUnit.optimizationPipeline)
    val loaded = exec.loadFunction(compiled)
    //val prog = generateProgram(50, 10)
    val setAdt = loaded.execute("main", Seq(prog1)).entries.head
    val query = Relation.from("Set$$TString_TString$$enum", Seq("$set", "$elem$0", "$elem$1"), Seq(Seq(setAdt, null, null)))
    val res = loaded.engine.read(query).project(1)
    assertResult(
      Set(("x", "x"), ("z", "y"), ("y", "y"), ("z", "x"), ("y", "x"), ("x", "y"))
    )(
      res.toSet
    )
  }