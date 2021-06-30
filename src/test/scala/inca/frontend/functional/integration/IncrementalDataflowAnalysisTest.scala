package inca.frontend.functional.integration

import inca.examples.functional.ControlDataFlow
import org.scalatest.funsuite.AnyFunSuite
import inca.frontend.functional.executor.IncrementalFunctionalExecutor._
import inca.runtime.EnginePool

class IncrementalDataflowAnalysisTest extends AnyFunSuite {
  // TODO fix slow update time tests
  test("Change rhs of assignment within loop") {
    testIncrementalRun(ControlDataFlow.exampleDataflow1, ControlDataFlow.exampleDataflow1Change1)
  }
  test("Insert y = y within loop after assignment of y") {
    testIncrementalRun(ControlDataFlow.exampleDataflow1, ControlDataFlow.exampleDataflow1Change2)
  }
  test("Change rhs of initial assignment of x (2 -> 3)") {
    testIncrementalRun(ControlDataFlow.exampleDataflow1, ControlDataFlow.exampleDataflow1Change3)
  }
  test("Insert assignment that changes value of x after loop") {
    testIncrementalRun(ControlDataFlow.exampleDataflow1, ControlDataFlow.exampleDataflow1Change7)
  }

  // fast update time tests
  test("Introduce assignment of new variable before loop") {
    testIncrementalRun(ControlDataFlow.exampleDataflow1, ControlDataFlow.exampleDataflow1Change4)
  }
  test("Introduce assignment of variable in loop (assign to static value)") {
    testIncrementalRun(ControlDataFlow.exampleDataflow1, ControlDataFlow.exampleDataflow1Change5)
  }
  test("Introduce assignment of variable in loop (assign to y)") {
    testIncrementalRun(ControlDataFlow.exampleDataflow1, ControlDataFlow.exampleDataflow1Change6)
  }

  def testIncrementalRun(original: meta.Term, changed: meta.Term): Unit = {
    val compiled = compileFunction(ControlDataFlow.IntValuesModule)
    val fun = loadFunction(compiled)
    val (edits, tuple) = fun.input(original)
    val (load, insert, delete) = fun.measureInitial("final_var", edits, tuple)
    println(s"IN ${load / 1000 / 1000}, ${insert / 1000 / 1000}, ${delete / 1000 / 1000}")

    // incremental measurement
    val (edits1, tuple1) = fun.input(changed)
    val (load1, insert1, delete1) = fun.measureUpdate("final_var", edits1, tuple1)

    // measure revert of change
    val (edits2, tuple2) = fun.input(original)
    val (load2, insert2, delete2) = fun.measureUpdate("final_var", edits2, tuple2)
    println(s"${insert1 / 1000 / 1000}, ${insert2 / 1000 / 1000}")
    EnginePool.disposeAllEngines()
  }
}
