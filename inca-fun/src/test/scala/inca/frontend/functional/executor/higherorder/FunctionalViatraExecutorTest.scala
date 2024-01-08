package inca.frontend.functional.executor.higherorder

import inca.frontend.functional.compile.{CompiledFunctionalModule, FunctionalCompilerOptions}
import inca.frontend.functional.executor.FunctionalExecutor
import inca.util.FileUtil
import inca.ir.execution.Relation
import org.scalatest.funsuite.AnyFunSuite


class FunctionalViatraExecutorTest extends AnyFunSuite:
  val options = FunctionalCompilerOptions.fromResource("functional/Options.ini")
  val exec: FunctionalExecutor = new FunctionalExecutor(new inca.viatra.Executor)

  test("Apply") {
    val code = FileUtil.readFileFromResource("functional/higherorder/Apply.finca")
    val compiled = exec.compileFunction(code, options)
    compiled.setPipeline(CompiledFunctionalModule.pipeline)
    val loaded = exec.loadFunction(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(6)(res.entries.head)
  }

  test("Compose Fun") {
    val code = FileUtil.readFileFromResource("functional/higherorder/ComposeFun.finca")
    val compiled = exec.compileFunction(code, options)
    compiled.setPipeline(CompiledFunctionalModule.pipeline)
    val loaded = exec.loadFunction(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(16)(res.entries.head)
  }

  test("Compose Lambda") {
    val code = FileUtil.readFileFromResource("functional/higherorder/ComposeLambda.finca")
    val compiled = exec.compileFunction(code, options)
    compiled.setPipeline(CompiledFunctionalModule.pipeline)
    val loaded = exec.loadFunction(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(16)(res.entries.head)
  }

  test("Lambda") {
    val code = FileUtil.readFileFromResource("functional/higherorder/Lambda.finca")
    val compiled = exec.compileFunction(code, options)
    compiled.setPipeline(CompiledFunctionalModule.pipeline)
    val loaded = exec.loadFunction(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(21)(res.entries.head)
  }

  test("Lambda Higher order") {
    val code = FileUtil.readFileFromResource("functional/higherorder/LambdaHigherOrder.finca")
    val compiled = exec.compileFunction(code, options)
    compiled.setPipeline(CompiledFunctionalModule.pipeline)
    val loaded = exec.loadFunction(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(63)(res.entries.head)
  }

  test("Transitive") {
    val code = FileUtil.readFileFromResource("functional/higherorder/Transitive.finca")
    val compiled = exec.compileFunction(code, options)
    compiled.setPipeline(CompiledFunctionalModule.pipeline)
    val loaded = exec.loadFunction(compiled)
    var res = loaded.execute("foo", Seq())
    var setAdt = res.entries.head
    var query = Relation.from("Set$$TInt_TInt$$$enum", Seq("$set", "$elem"), Seq(Seq(setAdt, null)))
    res = loaded.engine.read(query).project(1)
    assertResult(Set((2, 2), (2, 1), (1, 2), (1, 1), (3, 2), (3, 1), (3, 3), (2, 3), (1, 3)))(res.toSet)

    res = loaded.execute("bar", Seq())
    setAdt = res.entries.head
    query = Relation.from("Set$$TInt_TInt$$$enum", Seq("$set", "$elem"), Seq(Seq(setAdt, null)))
    res = loaded.engine.read(query).project(1)
    assertResult(Set((2, 2), (4, 1), (1, 2), (1, 4), (1, 1), (2, 4), (3, 2), (3, 1), (3, 3), (4, 3), (2, 3), (1, 3), (3, 4), (2, 1), (4, 2), (4, 4)))(res.toSet)
  }