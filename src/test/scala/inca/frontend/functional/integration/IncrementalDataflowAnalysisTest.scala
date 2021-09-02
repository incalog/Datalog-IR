package inca.frontend.functional.integration

import inca.examples.functional.ControlDataFlow
import inca.frontend.functional.executor.IncrementalFunctionalExecutor
import inca.frontend.functional.executor.IncrementalFunctionalExecutor._
import inca.runtime.{EnginePool, Query}
import org.eclipse.viatra.query.runtime.api.IMatchUpdateListener
import org.scalatest.Ignore
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable.ListBuffer

@Ignore
class IncrementalDataflowAnalysisTest extends AnyFunSuite {
  val defaultIntervalBound = 100
  val defaultIntervalInfty = 5000
  // TODO fix slow update time tests
  test("Change rhs of assignment within loop") {
    testIncrementalRun(ControlDataFlow.exampleDataflow1, ControlDataFlow.exampleDataflow1Change1)
  }

  test("Insert y = y within loop after assignment of y") {
    testIncrementalRun(ControlDataFlow.exampleDataflow1, ControlDataFlow.exampleDataflow1Change2)
  }
  test("Insert assignment that changes value of x after loop") {
    testIncrementalRun(ControlDataFlow.exampleDataflow1, ControlDataFlow.exampleDataflow1Change7)
  }
  test("Change rhs of initial assignment of x (2 -> 3)") {
    testIncrementalRun(ControlDataFlow.exampleDataflow1, ControlDataFlow.exampleDataflow1Change3)
  }
  test("Change rhs of initial assignment of x (99 -> 100) small example") {
    testIncrementalRun(ControlDataFlow.exampleDataflow7, ControlDataFlow.exampleDataflow7Change1)
  }
  test("Change rhs of initial assignment of x (99 -> 98) small example") {
    testIncrementalRun(ControlDataFlow.exampleDataflow7, ControlDataFlow.exampleDataflow7Change2)
  }

  test("Mininmal loop, initializer x=1 -> x=2") {
    testIncrementalRun(ControlDataFlow.exampleDataflow8, ControlDataFlow.exampleDataflow8Change1)
  }
  test("Mininmal loop, initializer x=1 -> x=0") {
    testIncrementalRun(ControlDataFlow.exampleDataflow8, ControlDataFlow.exampleDataflow8Change2)
  }
  test("Mininmal loop, add skip after increment") {
    testIncrementalRun(ControlDataFlow.exampleDataflow8, ControlDataFlow.exampleDataflow8Change3, 5, 1000)
  }
  test("Mininmal loop, add skip before increment") {
    testIncrementalRun(ControlDataFlow.exampleDataflow8, ControlDataFlow.exampleDataflow8Change4, 5, 1000)
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

  def testIncrementalRun(original: meta.Term, changed: meta.Term, intervalBound: Int = this.defaultIntervalBound, intervalInfty: Int = this.defaultIntervalInfty): Unit = {
    val compiled = compileFunction(ControlDataFlow.ParametricIntValuesModule(intervalBound, defaultIntervalInfty))

    for (i <- 0 until 5) {
      val fun = loadFunction(compiled)
      val (edits, tuple) = fun.input(original)
      val (load, insert, delete, m0) = fun.measureInitial("final_var", edits, tuple)
      println(s"Initial ${load / 1000 / 1000}, ${insert / 1000 / 1000}, ${delete / 1000 / 1000}")
    }
    EnginePool.disposeAllEngines()


    val fun = loadFunction(compiled)
//    println(fun.compiled.optimized)
    fun.registerTrackedRelations(Set("final_var"))

    val (edits, tuple) = fun.input(original)
    val (load, insert, delete, m0) = fun.measureInitial("final_var", edits, tuple)
    println(s"Initial ${load / 1000 / 1000}, ${insert / 1000 / 1000}, ${delete / 1000 / 1000}")
//    IncrementalFunctionalExecutor.printChanges(changes)
    println()

    for (i <- 0 until 10) {
      // incremental measurement
      val (edits1, tuple1) = fun.input(changed)
      edits1.print()
      val (load1, insert1, delete1, m1) = fun.measureUpdate("final_var", edits1, tuple1)
      println(s"Change1 ${load1 / 1000 / 1000}, ${insert1 / 1000 / 1000}, ${delete1 / 1000 / 1000}")
      fun.printChanges()
      println()

      // measure revert of change
      val (edits2, tuple2) = fun.input(original)
      val (load2, insert2, delete2, m2) = fun.measureUpdate("final_var", edits2, tuple2)
      println(s"Change2 ${load2 / 1000 / 1000}, ${insert2 / 1000 / 1000}, ${delete2 / 1000 / 1000}")
      fun.printChanges()
      println()
    }

    EnginePool.disposeAllEngines()
  }
}
