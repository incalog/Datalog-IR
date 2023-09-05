package inca.debugger.ir

import inca.backend.ir.Datalog
import inca.compiler.Compiler
import inca.compiler.Options
import inca.debugger._
import inca.measurements.util.CSVUtil.csvToString
import inca.measurements.util.CSVUtil.CSV
import inca.measurements.util.Config
import inca.measurements.util.MemoryUtil
import inca.runtime.context.DataModel
import inca.runtime.context.QueryScope
import inca.runtime.db.DatabaseInput
import inca.runtime.DatalogRuntime
import inca.runtime.EnginePool
import inca.util.FilesUtil
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples
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
    for (i <- 10 to 100 by 10) yield {
      PathConfig(
        5,
        20,
        "path",
        ValueTable(Seq("from", "temp"), Seq(Seq(ScalaValue(10), ScalaValue(11)))),
        i)
    }
  val overConfigs: Seq[PathConfig] =
    for (i <- 10 to 1000 by 10) yield {
      PathConfig(
        5,
        20,
        "path",
        ValueTable(Seq("from", "temp"), Seq(Seq(ScalaValue(10), ScalaValue(11)))),
        i)
    }

  def main(args: Array[String]): Unit = {
    // measure bottom-up time
    println("Bottom-Up")
    val buTime: mutable.ListBuffer[(Int, IndexedSeq[Long])] = mutable.ListBuffer()
    for (c <- overConfigs) {
      for (_ <- 0 until c.warmup) {
        val module = Datalog.Module("Prog", Seq(), Seq(ExamplePrograms.pathPatternExt), Seq())
        val blModule = BlacklistTransformation.transformer(DataModel.from()).transformModule(module)
        val runtime = initRuntime(blModule)
        measureBottomUp(runtime, graphFromPaper(c.numCycleNodes))
        EnginePool.disposeAllEngines()
        System.gc()
      }
      val measurements = for (_ <- 0 until c.runs) yield {
        val module = Datalog.Module("Prog", Seq(), Seq(ExamplePrograms.pathPatternExt), Seq())
        val blModule = BlacklistTransformation.transformer(DataModel.from()).transformModule(module)
        val runtime = initRuntime(blModule)
        val time = measureBottomUp(runtime, graphFromPaper(c.numCycleNodes))
        EnginePool.disposeAllEngines()
        System.gc()
        time
      }
      buTime += c.numCycleNodes -> measurements
    }
    FilesUtil.writeFile(s"$resultPath/Path-BUTime.csv", csvToString(toCSV(buTime.toSeq)))

    // measure time and number of steps (step-into)
    println("Into")
    val intoTime: mutable.ListBuffer[(Int, IndexedSeq[Long])] = mutable.ListBuffer()
    val intoSteps: mutable.ListBuffer[(Int, IndexedSeq[Long])] = mutable.ListBuffer()
    for (c <- intoConfigs) {
      println(s"Node ${c.numCycleNodes}")
      val measurements = measure(c, (debugger, _, _) => measureStepInto(debugger))
      intoTime += c.numCycleNodes -> measurements.map(_(1).asInstanceOf[Long])
      intoSteps += c.numCycleNodes -> measurements.map(_(0).asInstanceOf[Long])
    }
    FilesUtil.writeFile(s"$resultPath/Path-IntoTime-UnOpt.csv", csvToString(toCSV(intoTime.toSeq)))
    FilesUtil.writeFile(
      s"$resultPath/Path-IntoSteps-UnOpt.csv",
      csvToString(toCSV(intoSteps.toSeq)))

    // measure time and number of steps (step-over)
    println("Over")
    val overTime: mutable.ListBuffer[(Int, IndexedSeq[Long])] = mutable.ListBuffer()
    val overSteps: mutable.ListBuffer[(Int, IndexedSeq[Long])] = mutable.ListBuffer()
    for (c <- overConfigs) {
      println(s"Node ${c.numCycleNodes}")
      val measurements =
        measure(c, (debugger, _, _) => measureStepOver(debugger, c.predToStopAt, c.tableToStopAt))
      overTime += c.numCycleNodes -> measurements.map(_(1).asInstanceOf[Long])
      overSteps += c.numCycleNodes -> measurements.map(_(0).asInstanceOf[Long])
    }
    FilesUtil.writeFile(s"$resultPath/Path-OverTime-UnOpt.csv", csvToString(toCSV(overTime.toSeq)))
    FilesUtil.writeFile(
      s"$resultPath/Path-OverSteps-UnOpt.csv",
      csvToString(toCSV(overSteps.toSeq)))
  }

  def toCSV(vals: Seq[(Int, IndexedSeq[Long])]): CSV = {
    val header = vals.map(_._1).toIndexedSeq
    // we assume that each list has same number of elements
    val rowLength = vals.head._2.size
    val rows = for (i <- 0 until rowLength) yield vals.map(_._2(i)).toIndexedSeq
    header +: rows
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
    println("Warmup")
    for (i <- 0 until config.warmup) {
      val debugger = initDebugger(graphFromPaper(config.numCycleNodes))
      val queryTable = ValueTable(Seq("from"), Seq(Seq(ScalaValue(1))))
      debugger.entry("path", queryTable)
      measureClosure(debugger, config.predToStopAt, config.tableToStopAt)
    }
    println("Measure")
    val rows = for (i <- 0 until config.runs) yield {
      val debugger = initDebugger(graphFromPaper(config.numCycleNodes))
      val queryTable = ValueTable(Seq("from"), Seq(Seq(ScalaValue(1))))
      debugger.entry("path", queryTable)
      val (vals, steps, over) = measureClosure(debugger, config.predToStopAt, config.tableToStopAt)
      println("NEXT RUN")
      IndexedSeq[Any](steps, vals.sum + over.getOrElse(0L))
    }
    EnginePool.disposeAllEngines()
    MemoryUtil.collectGarbage()
    rows
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
        rt => new DelayingDebuggerState(rt)
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
