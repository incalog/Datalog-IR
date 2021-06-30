package inca.frontend.functional.integration

import inca.examples.functional.ControlDataFlow
import inca.frontend.functional.executor.IncrementalFunctionalExecutor._
import inca.runtime.EnginePool
import org.scalatest.funsuite.AnyFunSuite

class IncrementalDataflowAnalysisTest extends AnyFunSuite {
  val bound = 10
  val default = 1000
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
    val compiled = compileFunction(ControlDataFlow.ParametricIntValuesModule(bound, default))
    val fun = loadFunction(compiled)
//    println(fun.compiled.psystemSource)

//    val changes: ListBuffer[(Query.Match, Boolean)] = ListBuffer()
//    fun.engine.addMatchUpdateListener(fun.engine.getMatcher(compiled.psystemModule.patterns("VNum")()), new IMatchUpdateListener[Query.Match] {
//      override def notifyAppearance(mtch: Query.Match): Unit = changes += ((mtch, true))
//      override def notifyDisappearance(mtch: Query.Match): Unit = changes += ((mtch, false))
//    }, false)

    val (edits, tuple) = fun.input(original)
    val (load, insert, delete, m0) = fun.measureInitial("final_var", edits, tuple, true)
    println(s"Initial ${load / 1000 / 1000}, ${insert / 1000 / 1000}, ${delete / 1000 / 1000}")
//    IncrementalFunctionalExecutor.printChanges(changes)
//    changes.clear()

    // incremental measurement
    val (edits1, tuple1) = fun.input(changed)
    val (load1, insert1, delete1, m1) = fun.measureUpdate("final_var", edits1, tuple1, true)
    println(s"Change1 ${load1 / 1000 / 1000}, ${insert1 / 1000 / 1000}, ${delete1 / 1000 / 1000}")
    edits1.print()
//    IncrementalFunctionalExecutor.printChanges(changes)
//    changes.clear()

    // measure revert of change
    val (edits2, tuple2) = fun.input(original)
    val (load2, insert2, delete2, m2) = fun.measureUpdate("final_var", edits2, tuple2)
    println(s"Change2 ${load2 / 1000 / 1000}, ${insert2 / 1000 / 1000}, ${delete2 / 1000 / 1000}")
    EnginePool.disposeAllEngines()
  }
}
