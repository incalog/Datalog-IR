package inca.frontend.functional.incremental

import inca.examples.functional.ControlDataFlow
import inca.frontend.functional.executor.FunctionalExecutor.{compileFunction, loadFunction}
import inca.runtime.EnginePool
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuple
import truechange.EditScript

object RunInitial extends App {
  val compiled = compileFunction(ControlDataFlow.IntValuesModule)
  val runs = 100
  var edits: EditScript = null
  var tuple: Tuple = null
  for (i <- 0 until runs) {
    val fun = loadFunction(compiled)
    if (edits == null) {
      val input = fun.input(ControlDataFlow.exampleDataflow1)
      edits = input._1
      tuple = input._2
    }
    val (load, insert, delete) = fun.measure("final_var", edits, tuple)
    println(s"${load / 1000 / 1000}, ${insert / 1000 / 1000}, ${delete / 1000 / 1000}")
    EnginePool.disposeAllEngines()
  }
}

object RunIncremental extends App {
  import inca.frontend.functional.executor.IncrementalFunctionalExecutor

  val compiled = IncrementalFunctionalExecutor.compileFunction(ControlDataFlow.IntValuesModule)
  println(compiled.optimized)
  val runs = 100
  val fun = IncrementalFunctionalExecutor.loadFunction(compiled)
  val originProg = ControlDataFlow.exampleDataflow1
  val (edits, tuple) = fun.input(originProg)
  val (load, insert, delete, _) = fun.measureInitial("final_var", edits, tuple)
  println(s"IN ${load / 1000 / 1000}, ${insert / 1000 / 1000}, ${delete / 1000 / 1000}")
  // incremental measurements
  for (i <- 0 until runs) {
    // measure change
    val changedProg = ControlDataFlow.exampleDataflow1Change1
    val (edits1, tuple1) = fun.input(changedProg)
    val (load1, insert1, delete1, _) = fun.measureUpdate("final_var", edits1, tuple1)

    // measure revert of change
    val (edits2, tuple2) = fun.input(originProg)
    val (load2, insert2, delete2, _) = fun.measureUpdate("final_var", edits2, tuple2)
    println(s"${insert1 / 1000 / 1000}, ${insert2 / 1000 / 1000}")
  }
  EnginePool.disposeAllEngines()
}
