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
import inca.util.FilesUtil
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples
import scala.collection.mutable
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

  val resultPath = "benchmark-results/debugger"

  def main(args: Array[String]): Unit = {
    val config = PathConfig(
      1,
      1,
      "path",
      ValueTable(Seq("from", "temp"), Seq(Seq(ScalaValue(9), ScalaValue(10)))),
      100)
    val stepIntoMeasurements = measure(config, (debugger, _, _) => measureStepInto(debugger))
    val stepOverMeasurements =
      measure(config, (debugger, pred, t) => measureStepOver(debugger, pred, t))
    FilesUtil.writeFile(
      s"$resultPath/Path-StepInto${config.numCycleNodes}.csv",
      BenchmarkUtils.measurementsToCSV(stepIntoMeasurements))
    FilesUtil.writeFile(
      s"$resultPath/Path-StepOver${config.numCycleNodes}.csv",
      BenchmarkUtils.measurementsToCSV(stepOverMeasurements))
  }

  type Edge = (Int, Int)
  def graphFromPaper(x: Int): Seq[Edge] = line(1, 10) ++ cycle(10, x)
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
      measureClosure: (IRDebugger, String, ValueTable) => (Seq[Long], Long, Option[Long])
    ): Seq[Measurement] = {
    val debugger = initDebugger(graphFromPaper(config.numCycleNodes))
    for (i <- 0 until config.warmup) {
      val queryTable = ValueTable(Seq("from"), Seq(Seq(ScalaValue(1))))
      debugger.entry("path", queryTable)
      measureClosure(debugger, config.predToStopAt, config.tableToStopAt)
      debugger.state.clear()
      debugger.queryStack.clear()
      debugger.clearIRControlTrace()
    }
    for (i <- 0 until config.runs) yield {
      val queryTable = ValueTable(Seq("from"), Seq(Seq(ScalaValue(1))))
      debugger.entry("path", queryTable)
      val (vals, steps, over) = measureClosure(debugger, config.predToStopAt, config.tableToStopAt)
      debugger.state.clear()
      debugger.queryStack.clear()
      debugger.clearIRControlTrace()
      val extra =
        (if (over.isEmpty) Map.empty[String, Any]
         else Map("StepOver Time (ns)" -> over.get)) ++ Map(
          "NumberOfSteps" -> steps,
          "CompleteTime (ns)" -> vals.sum)

      Measurement(s"${config.numCycleNodes}", Units.Nanoseconds, vals, extra = extra)(
        BenchmarkUtils.Timing(config.warmup, config.runs))
    }
  }

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

  def measureStepInto(debugger: IRDebugger): (Seq[Long], Long, Option[Long]) = {
    val measurements: mutable.ListBuffer[Long] = mutable.ListBuffer()
    while (!debugger.isFinished) {
      val start = System.nanoTime()
      debugger.stepInto()
      val end = System.nanoTime()
      measurements += (end - start)
    }
    (measurements.toSeq, debugger.irControlTrace.size, None)
  }

  def measureStepOver(
      debugger: IRDebugger,
      predicateToStopAt: String,
      tableToStopAt: ValueTable
    ): (Seq[Long], Long, Option[Long]) = {
    var pathRecDepthQueryFound = false
    val measurements: mutable.ListBuffer[Long] = mutable.ListBuffer()
    while (!pathRecDepthQueryFound) {
      val start = System.nanoTime()
      debugger.stepInto()
      val end = System.nanoTime()
      measurements += end - start
      debugger.queryStack.top match {
        case Subquery(_, _, _, sup, Rule(_, _, Atom(Datalog.Call(p, _, _, _)) +: _) +: _) =>
          if (p == predicateToStopAt && sup.join(tableToStopAt).nonEmpty) {
            pathRecDepthQueryFound = true
          }
        case _ => // do nothing
      }
    }
    val start = System.nanoTime()
    debugger.stepOver()
    val end = System.nanoTime()
    val stepOverMeasurement = end - start
    measurements += stepOverMeasurement
    while (!debugger.isFinished) {
      val start = System.nanoTime()
      debugger.stepInto()
      val end = System.nanoTime()
      measurements += end - start
    }
    (measurements.toSeq, debugger.irControlTrace.size, Some(stepOverMeasurement))
  }
}
