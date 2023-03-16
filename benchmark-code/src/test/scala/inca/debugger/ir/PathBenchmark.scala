package inca.debugger.ir

import inca.backend.ir.Datalog
import inca.compiler.Compiler
import inca.compiler.Options
import inca.debugger._
import inca.measurements.util.BenchmarkUtils
import inca.measurements.util.BenchmarkUtils.Measurement
import inca.measurements.util.CSVUtil.csvToString
import inca.measurements.util.CSVUtil.CSV
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

  val resultPath = "benchmark-results/debugger/data"

  def main(args: Array[String]): Unit = {
    val configs =
      for (i <- 500 to 500 by 10) yield {
        PathConfig(
          5,
          1,
          "path",
          ValueTable(Seq("from", "temp"), Seq(Seq(ScalaValue(9), ScalaValue(10)))),
          i)
      }
    for (config <- configs) {
      // measure time and number of steps (step-into)
//      val stepIntoMeasurements = measure(config, (debugger, _, _) => measureStepInto(debugger))
//      println(s"Nodes ${config.numCycleNodes}")
//      println(s"STEPINTO")
//      println(csvToString(stepIntoMeasurements))
//      FilesUtil.writeFile(
//        s"$resultPath/Path-StepInto${config.numCycleNodes}.csv",
//        csvToString(stepIntoMeasurements))
//      // measure time and number of steps (step-over)
      val stepOverMeasurements = measure(
        config,
        (debugger, _, _) => measureStepOver(debugger, config.predToStopAt, config.tableToStopAt))
      println(s"STEPOVER")
      println(csvToString(stepOverMeasurements))
      FilesUtil.writeFile(
        s"$resultPath/Path-StepOver${config.numCycleNodes}.csv",
        csvToString(stepOverMeasurements))
    }
//    val stepOverMeasurements =
//      measure(config, (debugger, pred, t) => measureStepOver(debugger, pred, t))
//    FilesUtil.writeFile(
//      s"$resultPath/Path-StepInto${config.numCycleNodes}.csv",
//      BenchmarkUtils.measurementsToCSV(stepIntoMeasurements))
//    FilesUtil.writeFile(
//      s"$resultPath/Path-StepOver${config.numCycleNodes}.csv",
//      BenchmarkUtils.measurementsToCSV(stepOverMeasurements))
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
    ): CSV = {
    val debugger = initDebugger(graphFromPaper(config.numCycleNodes))
    for (i <- 0 until config.warmup) {
      val queryTable = ValueTable(Seq("from"), Seq(Seq(ScalaValue(1))))
      debugger.entry("path", queryTable)
      measureClosure(debugger, config.predToStopAt, config.tableToStopAt)
      debugger.state.clear()
      debugger.queryStack.clear()
      debugger.clearIRControlTrace()
    }
    val header = IndexedSeq[Any]("numSteps", "measurement")
    val rows = for (i <- 0 until config.runs) yield {
      val queryTable = ValueTable(Seq("from"), Seq(Seq(ScalaValue(1))))
      debugger.entry("path", queryTable)
      val (vals, steps, over) = measureClosure(debugger, config.predToStopAt, config.tableToStopAt)
      debugger.state.clear()
      debugger.queryStack.clear()
      debugger.clearIRControlTrace()
      IndexedSeq[Any](steps, vals.sum + over.getOrElse(0L))
    }
    header +: rows
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
    val debugger =
      new InitializingIRDebugger(compiled, dbInput, rt => new AccumulatingDebuggerState(rt))
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
    (measurements.toSeq, debugger.irControlTrace.size - 1, None)
  }

  def measureStepOver(
      debugger: IRDebugger,
      predicateToStopAt: String,
      tableToStopAt: ValueTable
    ): (Seq[Long], Long, Option[Long]) = {
    val measurements: mutable.ListBuffer[Long] = mutable.ListBuffer()
    while (!debugger.isFinished) {
      debugger.queryStack.top match {
        case Subquery(_, _, _, sup, Rule(_, _, Atom(Datalog.Call(p, _, _, _)) +: _) +: _) =>
          if (p == predicateToStopAt && sup.join(tableToStopAt).nonEmpty) {
            // step over
            val start = System.nanoTime()
            debugger.stepOver()
            val end = System.nanoTime()
            measurements += end - start
          } else {
            // step into
            val start = System.nanoTime()
            debugger.stepInto()
            val end = System.nanoTime()
            measurements += end - start
          }
        case _ =>
          // step into
          val start = System.nanoTime()
          debugger.stepInto()
          val end = System.nanoTime()
          measurements += end - start
      }
    }
//    val stepOverMeasurement = end - start
//    measurements += stepOverMeasurement
//    while (!debugger.isFinished) {
//      val start = System.nanoTime()
//      debugger.stepInto()
//      val end = System.nanoTime()
//      measurements += end - start
//    }
    (measurements.toSeq, debugger.irControlTrace.size - 1, None)
  }
}
