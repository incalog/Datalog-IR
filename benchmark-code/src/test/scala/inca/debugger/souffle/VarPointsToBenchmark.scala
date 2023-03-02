package inca.debugger.souffle

import inca.compiler.CompiledDatalogModule
import inca.compiler.Compiler
import inca.compiler.Options
import inca.debugger.souffle.Configs.scenario2v1
import inca.debugger.souffle.Configs.BaseConfig
import inca.debugger.souffle.Configs.DoopProgram
import inca.debugger.ExternallyInitializableDebugger
import inca.measurements.util.BenchmarkUtils
import inca.measurements.util.BenchmarkUtils.Measurement
import inca.measurements.util.BenchmarkUtils.Timing
import inca.measurements.util.MemoryUtil
import inca.measurements.util.Units
import inca.runtime.context.QueryScope
import inca.runtime.DatalogRuntime
import inca.runtime.EnginePool
import inca.util.FilesUtil
import org.eclipse.viatra.query.runtime.rete.matcher.DRedReteBackendFactory
import scala.collection.mutable

// TODO measure how long it takes evaluating subquery using step into
// TODO measure how long it takes evaluating subquery using step-over (hybrid semantics)

// TODO What are the programs?
// TODO What are the scenarios?
object VarPointsToBenchmark {
  implicit val timing: Timing = Timing(0, 0, outliers = 0)
//  var stepIntoAndOver: Seq[BottomUpVTopDownConfig] = Seq()
//  var onlyStepOver: Seq[StepIntoVStepOverConfig] = Seq()

  val resultsPath: String = "benchmark-results/debugger/"

  def initRuntime(config: BaseConfig): (CompiledDatalogModule, DatalogRuntime) = {
    val compiled =
      Compiler.compileGP(config.compiled.ir, config.compiled.dataModel, Options())
    val scope = new QueryScope(config.compiled.dataModel)
    val (_engine, _database) =
      // EnginePool.loadEngineAndDatabase(scope, DRedReteBackendFactory.INSTANCE)
      EnginePool.loadEngineAndDatabase(scope, DRedReteBackendFactory.INSTANCE)
    (compiled, DatalogRuntime(_engine, _database, compiled))
  }

  // returns running time and memory
  def measureBottomUp(config: BaseConfig): (Long, Long) = {
    val (compiled, runtime) = initRuntime(config)
    // measure running time
    val matcher = runtime.engine.getMatcher(compiled.psystemModule.patterns(config.entry)())
    val start = System.currentTimeMillis()
    runtime.engine.delayUpdatePropagation { () =>
      runtime.db.processDatabaseInput(config.input)
    }
    val end = System.currentTimeMillis()
    val endCount = System.currentTimeMillis()
    // measure memory
    MemoryUtil.collectGarbage()
    val mem = MemoryUtil.usedMemoryInMBytes()

    EnginePool.disposeAllEngines()
    (end - start, mem)
  }

  def initBottomUp(
      compiled: CompiledDatalogModule,
      runtime: DatalogRuntime,
      config: BaseConfig
    ): Unit = {
    runtime.engine.delayUpdatePropagation { () =>
      runtime.db.processDatabaseInput(config.input)
    }
    val matcher = runtime.engine.getMatcher(compiled.psystemModule.patterns(config.entry)())
    matcher.getAllMatches
  }

  def measureStepInto(config: BaseConfig): (Seq[Long], Long, Long, Option[Long]) = {
    val (module, runtime) = initRuntime(config)
    val debugger = new ExternallyInitializableDebugger(module)
    // initialize bottom-up database
    initBottomUp(module, runtime, config)
    debugger.setRuntime(runtime)
    println("START")
    debugger.entry(config.entry, config.args)
    val measurements: mutable.ListBuffer[Long] = mutable.ListBuffer()
    while (!debugger.isFinished) {
      val start = System.nanoTime()
      debugger.stepInto()
      val end = System.nanoTime()
      // println(debugger.queryStack.top)
      measurements += end - start
    }

//    val result = debugger.queryStack.top.asInstanceOf[QueryResult].result
//    val expected = debugger.state.readBottomUp(config.entry, config.args)

//    println(result.size)
//    println(result)
//    println(expected.size)
//    println(expected)
//    val tooMuch = result.entries.diff(expected.entries)
//    val missing = expected.entries.diff(result.entries)
//    println(tooMuch)
//    println(missing)
//    assert(result == expected)
    MemoryUtil.collectGarbage()
    val mem = MemoryUtil.usedMemoryInMBytes()
//    println(s"STEPS: ${debugger.irControlTrace.size}")
//    println(s"MS/STEP: ${(end - start).toDouble / debugger.irControlTrace.size.toDouble}")
    EnginePool.disposeAllEngines()
    (measurements.toSeq, mem, debugger.irControlTrace.size, None)
  }

  def measureStepOut(config: BaseConfig): (Seq[Long], Long, Long, Option[Long]) = {
    val (module, runtime) = initRuntime(config)
    val debugger = new ExternallyInitializableDebugger(module)
    // initialize bottom-up database
    initBottomUp(module, runtime, config)
    debugger.setRuntime(runtime)
    // val expected = debugger.state.readBottomUp(config.entry, config.args)
    debugger.entry(config.entry, config.args)
    val start = System.currentTimeMillis()
    debugger.stepOut()
    val end = System.currentTimeMillis()
    // val result = debugger.queryStack.top.asInstanceOf[QueryResult].result

//    println(result.size)
//    println(result)
//    println(expected.size)
//    println(expected)
//    val tooMuch = result.entries.diff(expected.entries)
//    val missing = expected.entries.diff(result.entries)
//    println(tooMuch)
//    println(missing)
//    assert(result == expected)
    MemoryUtil.collectGarbage()
    val mem = MemoryUtil.usedMemoryInMBytes()
    EnginePool.disposeAllEngines()
    (Seq(end - start), mem, debugger.irControlTrace.size, Some(end - start))
  }

  def collectMeasurements(
      config: BaseConfig,
      f: BaseConfig => (Seq[Long], Long, Long, Option[Long])
    ): Measurement = {
    val (time, mem, steps, over) = f(config)
    val extra = Map("NumberOfSteps" -> steps, "TotalTime (ns)" -> time.sum, "Memory (MB)" -> mem) ++
      (if (over.isEmpty) Map()
       else Map("StepOver (ns)" -> over.get))
    Measurement(config.name, Units.Nanoseconds, time, extra = extra)
  }

  def measureIntoAndOver(config: BaseConfig): (Seq[Measurement], Seq[Measurement]) = {
    val intoMeasurements =
      BenchmarkUtils.measure(() => collectMeasurements(config, measureStepInto), config)
    val overMeasurements =
      BenchmarkUtils.measure(() => collectMeasurements(config, measureStepOut), config)
    (intoMeasurements, overMeasurements)
  }

  def main(args: Array[String]): Unit = {
    val (sc1Into, sc1Over) = measureIntoAndOver(scenario2v1(DoopProgram.MiniJavac))
    FilesUtil.writeFile(
      s"$resultsPath/${sc1Into.head.name}-StepInto.csv",
      BenchmarkUtils.measurementsToCSV(sc1Into))
    FilesUtil.writeFile(
      s"$resultsPath/${sc1Over.head.name}-StepOver.csv",
      BenchmarkUtils.measurementsToCSV(sc1Over))
  }
}
