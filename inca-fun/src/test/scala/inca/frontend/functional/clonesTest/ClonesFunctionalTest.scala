package inca.frontend.functional.clonesTest

import inca.frontend.functional.compile.CompiledFunctionalModule
import inca.frontend.functional.executor.FunctionalExecutor
import inca.frontend.functional.foreign
import inca.ir.execution.Relation
import inca.util.FileUtil
import org.scalatest.funsuite.AnyFunSuite

class ClonesFunctionalTest extends AnyFunSuite{
  val exec: FunctionalExecutor = new FunctionalExecutor(inca.viatra.Executor)

  test("simple redundant let") {
    val code = FileUtil.readFileFromResource("functional/clones/letRedundant.finca")
    val compiled = exec.compileFunction(code)
    compiled.setPipeline(CompiledFunctionalModule.pipeline)

    // TODO
    val vnResult = compiled.valueNumbering(compiled.lowered)
    assertResult(???)(vnResult)

    val loaded = exec.loadFunction(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(7)(res.entries.head)
  }

  test("simple redundant let 2 (with input)") {
    val code = FileUtil.readFileFromResource("functional/clones/letRedundant2.finca")
    val compiled = exec.compileFunction(code)
    compiled.setPipeline(CompiledFunctionalModule.pipeline)
    val loaded = exec.loadFunction(compiled)
    val res = loaded.execute("main", Seq(1))
    assertResult(1)(res.entries.head)
  }

  test("let with addition") {
    val code = FileUtil.readFileFromResource("functional/clones/letAddition.finca")
    val compiled = exec.compileFunction(code)
    compiled.setPipeline(CompiledFunctionalModule.pipeline)
    val loaded = exec.loadFunction(compiled)
    val res = loaded.execute("main", Seq(1))
    assertResult(33)(res.entries.head)
  }

  test("if Condition with Add") {
    val code = FileUtil.readFileFromResource("functional/clones/ifCondition.finca")
    val compiled = exec.compileFunction(code)
    compiled.setPipeline(CompiledFunctionalModule.pipeline)
    val loaded = exec.loadFunction(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(7)(res.entries.head)
  }

  test("if Condition2 (with input)") {
    val code = FileUtil.readFileFromResource("functional/clones/ifCondition2.finca")
    val compiled = exec.compileFunction(code)
    compiled.setPipeline(CompiledFunctionalModule.pipeline)
    val loaded = exec.loadFunction(compiled)
    val res = loaded.execute("main", Seq(7))
    assertResult(7)(res.entries.head)
  }

  test("if Condition3 (nested with input)") {
    val code = FileUtil.readFileFromResource("functional/clones/ifCondition3.finca")
    val compiled = exec.compileFunction(code)
    compiled.setPipeline(CompiledFunctionalModule.pipeline)
    val loaded = exec.loadFunction(compiled)
    val res = loaded.execute("main", Seq(1))
    assertResult(2)(res.entries.head)
  }

}
