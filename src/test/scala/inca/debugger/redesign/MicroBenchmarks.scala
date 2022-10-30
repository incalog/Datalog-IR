package inca.debugger.redesign

import inca.backend.ir.Datalog
import inca.compiler.Compiler
import inca.compiler.Options
import inca.debugger.table.ImmutableTable
import inca.debugger.ExamplePrograms
import inca.debugger.ScalaValue
import inca.debugger.Value
import inca.runtime.context.DataModel
import inca.runtime.db.DatabaseInput
import inca.util.measurement.BenchmarkUtils
import inca.util.measurement.BenchmarkUtils.Measurement
import inca.util.TimeTracker
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples
import truechange.EditScript

object MicroBenchmarks {

  def main(args: Array[String]): Unit = {
    val config = Config(
      0,
      3,
      "path",
      ImmutableTable[Value](Seq("from", "temp"), Seq(Seq(ScalaValue(9), ScalaValue(10)))),
      200)
    val stepIntoMeasurements = measure(config, (debugger, _, _) => measureStepInto(debugger))
    val stepOverMeasurements =
      measure(
        config,
        (debugger, predToStopAt, queryTableToStopAt) =>
          measureStepOver(debugger, predToStopAt, queryTableToStopAt))
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
      config: Config,
      measureClosure: (Debugger, String, ImmutableTable[Value]) => Long
    ): Measurement = {
    val debugger: Debugger = initDebugger(oneRandomAssGraph(config.numCycleNodes))
    val measurementInit = TimeTracker.measurementInMilli
    TimeTracker.clear()
    for (i <- 0 until config.warmup) {
      val queryTable = ImmutableTable[Value](Seq("from"), Seq(Seq(ScalaValue(0))))
      debugger.entry("path", queryTable)
      measureClosure(debugger, config.predicateToStopAt, config.queryTableToStopAt)
      debugger.state.clear()
      debugger.callStack.clear()
    }
    val measurements = for (i <- 0 until config.runs) yield {
      val queryTable = ImmutableTable[Value](Seq("from"), Seq(Seq(ScalaValue(0))))
      debugger.entry("path", queryTable)
      val measurement =
        measureClosure(debugger, config.predicateToStopAt, config.queryTableToStopAt)
      debugger.state.clear()
      debugger.callStack.clear()
      measurement
    }
    Measurement(s"${config.numCycleNodes}", measurements, Map("init (ns)" -> measurementInit))(
      BenchmarkUtils.Timing(config.warmup, config.runs))
  }

  // def initRuntime(edges: Seq[Edge]): DatalogRuntime
  def initDebugger(edges: Seq[Edge]): Debugger = {
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
    val debugger = new IRDebugger(compiled, dbInput)
    debugger
  }

  def stepIntoTillFinished(debugger: Debugger): Unit = {
    while (!debugger.isFinished)
      debugger.stepInto()
  }
  def measureStepInto(debugger: Debugger): Long = {
    val start = System.currentTimeMillis()
    stepIntoTillFinished(debugger)
    val end = System.currentTimeMillis()
    end - start
  }

  def measureStepOver(
      debugger: Debugger,
      predicateToStopAt: String,
      queryTable: ImmutableTable[Value]
    ): Long = {
    val start = System.currentTimeMillis()
    var pathRecDepthQueryFound = false
    while (!pathRecDepthQueryFound) {
      debugger.stepInto()
      debugger.callStack.top match {
        case InRule(_, _, _, RuleEvaluation(ruleRes, _, Datalog.Call(p, _, _, _) :: remAtoms), _) =>
          if (p == predicateToStopAt && ruleRes.join(queryTable).nonEmpty) {
            pathRecDepthQueryFound = true
          }
        case _ => // do nothing
      }
    }
    debugger.stepOver()
    stepIntoTillFinished(debugger)
    val end = System.currentTimeMillis()
    end - start
  }
}

case class Config(
    warmup: Int,
    runs: Int,
    predicateToStopAt: String,
    queryTableToStopAt: ImmutableTable[Value],
    numCycleNodes: Int)
