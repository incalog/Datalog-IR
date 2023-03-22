package inca.debugger.ir

import inca.backend.ir.Datalog
import inca.compiler.CompiledDatalogModule
import inca.compiler.Compiler
import inca.compiler.Options
import inca.debugger._
import inca.debugger.souffle.Configs.BaseConfig
import inca.measurements.util.BenchmarkUtils
import inca.measurements.util.BenchmarkUtils.Measurement
import inca.measurements.util.CSVUtil.csvToString
import inca.measurements.util.CSVUtil.CSV
import inca.measurements.util.Config
import inca.measurements.util.Units
import inca.runtime.context.DataModel
import inca.runtime.context.QueryScope
import inca.runtime.db.DatabaseInput
import inca.runtime.DatalogRuntime
import inca.runtime.EnginePool
import inca.util.FilesUtil
import org.eclipse.viatra.query.runtime.api.AdvancedViatraQueryEngine
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples
import org.eclipse.viatra.query.runtime.rete.matcher.DRedReteBackendFactory
import org.eclipse.viatra.query.runtime.rete.matcher.TimelyReteBackendFactory
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

  val intoConfigs: Seq[PathConfig] =
    for (i <- 10 to 500 by 10) yield {
      PathConfig(
        5,
        10,
        "path",
        ValueTable(Seq("from", "temp"), Seq(Seq(ScalaValue(10), ScalaValue(11)))),
        i)
    }
  val overConfigs: Seq[PathConfig] =
    for (i <- 10 to 500 by 10) yield {
      PathConfig(
        5,
        10,
        "path",
        ValueTable(Seq("from", "temp"), Seq(Seq(ScalaValue(10), ScalaValue(11)))),
        i)
    }

  def main(args: Array[String]): Unit = {
    // measure bottom-up time
    println("Bottom-Up")
//    for (c <- overConfigs) {
//      for (_ <- 0 until c.warmup) {
//        val module = Datalog.Module("Prog", Seq(), Seq(ExamplePrograms.pathPatternExt), Seq())
//        val blModule = BlacklistTransformation.transformer(DataModel.from()).transformModule(module)
//        val runtime = initRuntime(blModule)
//        measureBottomUp(runtime, graphFromPaper(c.numCycleNodes))
//        EnginePool.disposeAllEngines()
//        System.gc()
//      }
//      val measurements = for (_ <- 0 until c.runs) yield {
//        val module = Datalog.Module("Prog", Seq(), Seq(ExamplePrograms.pathPatternExt), Seq())
//        val blModule = BlacklistTransformation.transformer(DataModel.from()).transformModule(module)
//        val runtime = initRuntime(blModule)
//        val time = measureBottomUp(runtime, graphFromPaper(c.numCycleNodes))
//        EnginePool.disposeAllEngines()
//        System.gc()
//        time
//      }
//      println(measurements)
//      FilesUtil.writeFile(
//        s"$resultPath/Path-BottomUp${c.numCycleNodes}.csv",
//        csvToString(IndexedSeq("measurement") +: measurements.map(v => IndexedSeq(v))))
//    }
    // measure time and number of steps (step-into)
    println("Into")
    for (c <- intoConfigs) {
      println(s"Node ${c.numCycleNodes}")
      val stepIntoMeasurements = measure(c, (debugger, _, _) => measureStepInto(debugger))
      FilesUtil.writeFile(
        s"$resultPath/Path-StepInto${c.numCycleNodes}-Opt.csv",
        csvToString(stepIntoMeasurements))
    }

    // measure time and number of steps (step-over)
//    println("Over")
//    for (c <- overConfigs) {
//      println(s"Node ${c.numCycleNodes}")
//      val stepOverMeasurements =
//        measure(c, (debugger, _, _) => measureStepOver(debugger, c.predToStopAt, c.tableToStopAt))
//      FilesUtil.writeFile(
//        s"$resultPath/Path-StepOver${c.numCycleNodes}-Opt.csv",
//        csvToString(stepOverMeasurements))
//    }
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
    println("Init")
    val debugger = initDebugger(graphFromPaper(config.numCycleNodes))
    println("Warmup")
    for (i <- 0 until config.warmup) {
      val queryTable = ValueTable(Seq("from"), Seq(Seq(ScalaValue(1))))
      debugger.entry("path", queryTable)
      measureClosure(debugger, config.predToStopAt, config.tableToStopAt)
      debugger.state.clear()
      debugger.queryStack.clear()
      debugger.clearIRControlTrace()
    }
    val header = IndexedSeq[Any]("numSteps", "measurement")
    println("Measure")
    val rows = for (i <- 0 until config.runs) yield {
      val queryTable = ValueTable(Seq("from"), Seq(Seq(ScalaValue(1))))
      debugger.entry("path", queryTable)
      val (vals, steps, over) = measureClosure(debugger, config.predToStopAt, config.tableToStopAt)
      println("NEXT RUN")
      debugger.state.clear()
      debugger.queryStack.clear()
      debugger.clearIRControlTrace()
      IndexedSeq[Any](steps, vals.sum + over.getOrElse(0L))
    }
    EnginePool.disposeAllEngines()
    System.gc()
    header +: rows
  }

  def initDebugger(edges: Seq[Edge]): IRDebugger = {
    val module = Datalog.Module("Prog", Seq(), Seq(ExamplePrograms.pathPatternExt), Seq())
    val blModule = BlacklistTransformation.transformer(DataModel.from()).transformModule(module)
    val runtime = initRuntime(blModule)
    measureBottomUp(runtime, edges)

    val compiled = Compiler.compileGP(
      module,
      DataModel.from(),
      Options()
    )

    val debugger =
      new ExternallyInitializableDebugger(
        compiled,
        rt => new AccumulatingDebuggerState(rt) with AvoidNonProducingIterationDebuggerState
      )
    debugger.setRuntime(runtime)
    debugger
  }

  def initRuntime(module: Datalog.Module): DatalogRuntime = {
    val compiled = Compiler.compileGP(
      module,
      DataModel.from(),
      Options()
    )
    val scope = new QueryScope(compiled.dataModel)
//    val (engine, db) = EnginePool.loadEngineAndDatabase(scope, DRedReteBackendFactory.INSTANCE)
    val (engine, db) =
      EnginePool.loadEngineAndDatabase(scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
    DatalogRuntime(engine, db, compiled)
  }

  def measureBottomUp(runtime: DatalogRuntime, edges: Seq[Edge]): Long = {
    val insertions = Map("edge" -> edges.map { case (f, t) =>
      Tuples.staticArityFlatTupleOf(f, t)
    }.toSet)
    val dbInput = DatabaseInput(EditScript(Seq()), insertions, Map())
    val matcher = runtime.engine.getMatcher(runtime.compiled.psystemModule.patterns("path")())
    val start = System.nanoTime()
    runtime.engine.delayUpdatePropagation { () =>
      runtime.db.processDatabaseInput(dbInput)
    }
    matcher.countMatches()
    val end = System.nanoTime()
    end - start
  }

  def measureStepInto(debugger: IRDebugger): (Seq[Long], Long, Option[Long]) = {
    val measurements: mutable.ListBuffer[Long] = mutable.ListBuffer()
    while (!debugger.isFinished) {
      val start = System.nanoTime()
      debugger.stepInto()
      val end = System.nanoTime()
      measurements += (end - start)
    }
    println(debugger.queryStack.top)
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
    println(debugger.queryStack.top)
    (measurements.toSeq, debugger.irControlTrace.size - 1, None)
  }
}
