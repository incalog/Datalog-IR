package inca.debugger.ir

import inca.backend.ir.Datalog
import inca.compiler.Compiler
import inca.compiler.Options
import inca.debugger._
import inca.measurements.util.BenchmarkUtils
import inca.measurements.util.BenchmarkUtils.Measurement
import inca.measurements.util.Config
import inca.measurements.util.Units
import inca.runtime.context.DataModel
import inca.runtime.db.DatabaseInput
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples
import truechange.EditScript

case class PathConfig(
    warmup: Int,
    runs: Int,
    predToStopAt: String,
    tableToStopAt: ValueTable,
    numCycleNodes: Int)
    extends Config {
  def name: String = ""
}

object PathBenchmark {

  def main(args: Array[String]): Unit = {
    val config = PathConfig(
      0,
      1,
      "path",
      ValueTable(Seq("from", "temp"), Seq(Seq(ScalaValue(9), ScalaValue(10)))),
      200)
    val stepIntoMeasurements = measure(config, (debugger, _, _) => measureStepInto(debugger))
    val stepOverMeasurements =
      measure(
        config,
        (debugger, predToStopAt, queryTableToStopAt) =>
          measureStepOver(debugger, predToStopAt, queryTableToStopAt))
    println("STEPINTO")
    println(stepIntoMeasurements.toString)
    println("STEPOVER")
    println(stepOverMeasurements.toString)
    println(BenchmarkUtils.measurementsToCSV(Seq(stepIntoMeasurements)))
    println(BenchmarkUtils.measurementsToCSV(Seq(stepOverMeasurements)))
    //    BenchmarkUtils.writeFile(
    //      s"StepOver + ${config.numCycleNodes}.csv",
    //      BenchmarkUtils.measurementsToCSV(Seq(stepOverMeasurements)))
  }

  type Edge = (Int, Int)
  def oneRandomAssGraph(x: Int): Seq[Edge] = cycle(0, x)
  //    edge(1, 3) ++ edge(1, 5) ++
  //    edge(3, 2) ++
  //    edge(4, 0) ++ edge(4, 7) ++
  //    edge(5, 2) ++
  //    edge(8, 7) ++
  //    edge(9, 6)
  def edge(from: Int, to: Int): Seq[Edge] = Seq((from, to))
  // to - from edges
  def cycle(from: Int, to: Int): Seq[Edge] = line(from, to) :+ ((to, from))
  // to - from - 1 edges
  def line(from: Int, to: Int): Seq[Edge] = (from until to).map { i =>
    (i, i + 1)
  }
  // 1 edge
  def loop(n: Int): Seq[Edge] = cycle(n, n)

  def measure(
      config: PathConfig,
      measureClosure: (IRDebugger, String, ValueTable) => Long
    ): Measurement = {
    val debugger = initDebugger(oneRandomAssGraph(config.numCycleNodes))
    for (i <- 0 until config.warmup) {
      val queryTable = ValueTable(Seq("from"), Seq(Seq(ScalaValue(0))))
      debugger.entry("path", queryTable)
      measureClosure(debugger, config.predToStopAt, config.tableToStopAt)
      debugger.state.clear()
      debugger.queryStack.clear()
    }
    val measurements = for (i <- 0 until config.runs) yield {
      val queryTable = ValueTable(Seq("from"), Seq(Seq(ScalaValue(0))))
      debugger.entry("path", queryTable)
      val measurement =
        measureClosure(debugger, config.predToStopAt, config.tableToStopAt)
      debugger.state.clear()
      debugger.queryStack.clear()
      measurement
    }
    Measurement(s"${config.numCycleNodes}", Units.Milliseconds, measurements)(
      BenchmarkUtils.Timing(config.warmup, config.runs))
    // Map("init (ns)" -> measurementInit))(
  }

  // def initRuntime(edges: Seq[Edge]): DatalogRuntime
  def initDebugger(edges: Seq[Edge]): IRDebugger = {
    val module = Datalog.Module("Prog", Seq(), Seq(ExamplePrograms.pathPatternExt), Seq())
    val compiled = Compiler.compileGP(
      module,
      DataModel.from(),
      Options()
    ) // withEngine(DRedReteBackendFactory.INSTANCE))
    val insertions = Map("edge" -> edges.map { case (f, t) =>
      Tuples.staticArityFlatTupleOf(f, t)
    }.toSet)
    val dbInput = DatabaseInput(EditScript(Seq()), insertions, Map())
    val debugger = new InitializingIRDebugger(compiled, dbInput)
    debugger
  }

  def stepIntoTillFinished(debugger: IRDebugger): Unit = {
    while (!debugger.isFinished)
      debugger.stepInto()
  }
  def measureStepInto(debugger: IRDebugger): Long = {
    val start = System.currentTimeMillis()
    stepIntoTillFinished(debugger)
    val end = System.currentTimeMillis()
    println(s"STEPINTO-TIME (MS): ${end - start}")
    println(s"STEPINTO-STEPS: ${debugger.irControlTrace.size}")
    end - start
  }

  def measureStepOver(
      debugger: IRDebugger,
      predicateToStopAt: String,
      tableToStopAt: ValueTable
    ): Long = {
    val start = System.currentTimeMillis()
    var pathRecDepthQueryFound = false
    while (!pathRecDepthQueryFound) {
      debugger.stepInto()
      debugger.queryStack.top match {
        case Subquery(_, _, _, sup, Rule(_, _, Atom(Datalog.Call(p, _, _, _)) +: _) +: _) =>
          if (p == predicateToStopAt && sup.join(tableToStopAt).nonEmpty) {
            pathRecDepthQueryFound = true
          }
        case _ => // do nothing
      }
    }
    debugger.stepOver()
    stepIntoTillFinished(debugger)
    val end = System.currentTimeMillis()
    println(s"STEPOVER-TIME (MS): ${end - start}")
    println(s"STEPOVER-STEPS: ${debugger.irControlTrace.size}")
    end - start
  }
}
