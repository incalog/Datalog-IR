package inca.debugger.measurements

import inca.backend.ir.Datalog
import inca.compiler.CompiledDatalogModule
import inca.compiler.Compiler
import inca.compiler.Options
import inca.debugger.ExternallyInitializableDebugger
import inca.debugger.IRBreakpoint
import inca.debugger.IRDebugger
import inca.debugger.ValueTable
import inca.runtime.context.DataModel
import inca.runtime.context.QueryScope
import inca.runtime.db.DatabaseInput
import inca.runtime.DatalogRuntime
import inca.runtime.EnginePool
import inca.util.measurement.BenchmarkUtils
import inca.util.measurement.BenchmarkUtils.Measurement
import inca.util.measurement.BenchmarkUtils.Timing
import inca.util.measurement.MemoryUtil
import org.eclipse.viatra.query.runtime.rete.matcher.TimelyReteBackendFactory

object Benchmark {

  // TODO measure how long it takes evaluating subquery using step into
  // TODO measure how long it takes evaluating subquery using step-over (hybrid semantics)

  // TODO What are the programs?
  // TODO What are the scenarios?
  trait Config {
    val prog: Datalog.Module
    val dataModel: DataModel
    val input: DatabaseInput
    val entry: String
    val args: ValueTable
    val warmup: Int
    val runs: Int

    def name: String
  }
  case class BottomUpVTopDownConfig(
      prog: Datalog.Module,
      dataModel: DataModel,
      input: DatabaseInput,
      entry: String,
      args: ValueTable,
      warmup: Int,
      runs: Int)
      extends Config {
    def name: String = s"${prog.name}_${entry}"
  }
  case class StepIntoVStepOverConfig(
      prog: Datalog.Module,
      dataModel: DataModel,
      input: DatabaseInput,
      entry: String,
      args: ValueTable,
      warmup: Int,
      runs: Int)
      extends Config {
    def name: String = s"${prog.name}"
  }
  implicit val timing: Timing = Timing(0, 0, outliers = 0)

  val bottomUpVTopDownConfigs: Seq[BottomUpVTopDownConfig] = Seq()
  val stepIntoVStepOverConfigs: Seq[StepIntoVStepOverConfig] = Seq()

  val path: String = "benchmark/debugger/measurements.csv"

  def initRuntime(config: Config): (CompiledDatalogModule, DatalogRuntime) = {
    val compiled = Compiler.compileGP(config.prog, config.dataModel, Options())
    val scope = new QueryScope(config.dataModel)
    val (_engine, _database) =
      EnginePool.loadEngineAndDatabase(scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
    (compiled, DatalogRuntime(_engine, _database, compiled))
  }

  // returns running time and memory
  def measureBottomUp(config: Config): (Long, Long) = {
    val (_, runtime) = initRuntime(config)
    // measure running time
    val start = System.currentTimeMillis()
    runtime.db.processDatabaseInput(config.input)
    val end = System.currentTimeMillis()
    // measure memory
    MemoryUtil.collectGarbage()
    val memoryInBytes = MemoryUtil.usedMemoryInBytes()

    (end - start, memoryInBytes)
  }

  def measureStepInto(config: Config): (Long, Long) = {
    val (module, runtime) = initRuntime(config)
    val debugger = new ExternallyInitializableDebugger(module)
    debugger.setRuntime(runtime)
    debugger.entry(config.entry, config.args)
    val start = System.currentTimeMillis()
    while (!debugger.isFinished) {
      debugger.stepInto()
    }
    val end = System.currentTimeMillis()
    MemoryUtil.collectGarbage()
    val mem = MemoryUtil.usedMemoryInBytes()
    (end - start, mem)
  }

  def collectBottomUpMeasurements(config: Config): (Measurement, Measurement) = {
    warmup(() => measureBottomUp(config), config)
    val (time, mem) = run(() => measureBottomUp(config), config).unzip
    (
      Measurement("bottomup_time_" + config.name, time),
      Measurement("bottomup_mem_" + config.name, mem)
    )
  }

  def collectTopDownMeasurements(config: Config): (Measurement, Measurement) = {
    warmup(() => measureStepInto(config), config)
    val (time, mem) = run(() => measureBottomUp(config), config).unzip
    (
      Measurement("topdown_time_" + config.name, time),
      Measurement("topdown_mem_" + config.name, mem)
    )
  }

  def warmup(run: () => Unit, config: Config): Unit =
    for (_ <- 0 until config.warmup) {
      run()
    }

  def run[T](run: () => T, config: Config): Seq[T] =
    for (_ <- 0 until config.runs) yield {
      run()
    }

  def measureStepIntoVStepOver(config: StepIntoVStepOverConfig): Measurement = {
    null
  }

  def main(args: Array[String]): Unit = {
    val (buTime, buMem) = bottomUpVTopDownConfigs.map(collectBottomUpMeasurements).unzip
    BenchmarkUtils.writeFile(path, BenchmarkUtils.measurementsToCSV(buTime ++ buMem))
  }

}
