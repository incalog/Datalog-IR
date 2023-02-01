package inca.debugger.souffle

import inca.backend.ir.Datalog
import inca.compiler.source.SourceFile
import inca.compiler.CompiledDatalogModule
import inca.compiler.Compiler
import inca.compiler.Options
import inca.debugger.ExternallyInitializableDebugger
import inca.debugger.ValueTable
import inca.frontend.souffle.compiler.CompiledSouffleModule
import inca.frontend.souffle.lowering.SouffleToDatalogIR
import inca.frontend.souffle.lowering.SouffleToNamedRelations
import inca.frontend.souffle.parser.Parser
import inca.measurements.util.BenchmarkUtils
import inca.measurements.util.BenchmarkUtils.Measurement
import inca.measurements.util.BenchmarkUtils.Timing
import inca.measurements.util.Config
import inca.measurements.util.MemoryUtil
import inca.runtime.context.DataModel
import inca.runtime.context.QueryScope
import inca.runtime.db.DatabaseInput
import inca.runtime.DatalogRuntime
import inca.runtime.EnginePool
import inca.util.FilesUtil
import java.io.File
import org.eclipse.viatra.query.runtime.rete.matcher.TimelyReteBackendFactory

object Benchmark {

  // TODO measure how long it takes evaluating subquery using step into
  // TODO measure how long it takes evaluating subquery using step-over (hybrid semantics)

  // TODO What are the programs?
  // TODO What are the scenarios?
  trait BaseConfig extends Config {
    val prog: Datalog.Module
    val dataModel: DataModel
    val input: DatabaseInput
    val entry: String
    val args: ValueTable

  }
  case class BottomUpVTopDownConfig(
      prog: Datalog.Module,
      dataModel: DataModel,
      input: DatabaseInput,
      entry: String,
      args: ValueTable,
      warmup: Int,
      runs: Int)
      extends BaseConfig {
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
      extends BaseConfig {
    def name: String = s"${prog.name}"
  }
  implicit val timing: Timing = Timing(0, 0, outliers = 0)

  var bottomUpVTopDownConfigs: Seq[BottomUpVTopDownConfig] = Seq()
  var stepIntoVStepOverConfigs: Seq[StepIntoVStepOverConfig] = Seq()

  val timeResultsPath: String = "benchmark-results/debugger/measurements-time.csv"
  val memResultsPath: String = "benchmark-results/debugger/measurements-mem-old.csv"

  def initRuntime(config: BaseConfig): (CompiledDatalogModule, DatalogRuntime) = {
    val compiled = Compiler.compileGP(config.prog, config.dataModel, Options())
    val scope = new QueryScope(config.dataModel)
    val (_engine, _database) =
      EnginePool.loadEngineAndDatabase(scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
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
    println(matcher.countMatches())
    val endCount = System.currentTimeMillis()
    println(end - start)
    println(endCount - start)
    // measure memory
    MemoryUtil.collectGarbage()
    val memoryInBytes = MemoryUtil.usedMemoryInBytes()

    EnginePool.disposeAllEngines()
    (end - start, memoryInBytes)
  }

  def measureStepInto(config: BaseConfig): (Long, Long) = {
    val (module, runtime) = initRuntime(config)
    val debugger = new ExternallyInitializableDebugger(module)
    // initialize bottom-up database
    runtime.engine.delayUpdatePropagation { () =>
      runtime.db.processDatabaseInput(config.input)
    }
    debugger.setRuntime(runtime)
    debugger.entry(config.entry, config.args)
    val start = System.currentTimeMillis()
    while (!debugger.isFinished) {
      debugger.stepInto()
    }
    val end = System.currentTimeMillis()
    println(debugger.queryStack.top)
    MemoryUtil.collectGarbage()
    val mem = MemoryUtil.usedMemoryInBytes()

    EnginePool.disposeAllEngines()
    (end - start, mem)
  }

  def collectBottomUpMeasurements(config: BaseConfig): (Measurement, Measurement) = {
    warmup(() => measureBottomUp(config), config)
    val (time, mem) = run(() => measureBottomUp(config), config).unzip
    (
      Measurement("bottomup_time_" + config.name, time),
      Measurement("bottomup_mem_" + config.name, mem)
    )
  }

  def collectTopDownMeasurements(config: BaseConfig): (Measurement, Measurement) = {
    warmup(() => measureStepInto(config), config)
    val (time, mem) = run(() => measureStepInto(config), config).unzip
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

  // TODO
  def measureStepIntoVStepOver(config: StepIntoVStepOverConfig): Measurement = {
    null
  }

  def readSouffleProgram(path: String): CompiledSouffleModule = {
    val file = new File(path)
    val analysis = Parser.parse(SourceFile(file.toPath))
    val compiler = new SouffleToDatalogIR(false)
    compiler.compile("SouffleModule", analysis)
  }

  def readSouffleInput(compiled: CompiledSouffleModule, path: String): DatabaseInput = {
    val inputCompiler = new SouffleToNamedRelations(path)
    inputCompiler.compile(compiled.inputs.values.map(x => x._2 -> x._1).toMap)
  }

  def varPointsToConfig(
      inputPath: String,
      entry: String,
      args: ValueTable,
      warmup: Int,
      runs: Int
    ): BottomUpVTopDownConfig = {
    val compiledModule = readSouffleProgram("souffle-frontend/benchmark/self-contained.dl")
    val input = readSouffleInput(compiledModule, inputPath)
    BottomUpVTopDownConfig(
      compiledModule.ir,
      compiledModule.dataModel,
      input,
      entry,
      args,
      warmup,
      runs)
  }

  def main(args: Array[String]): Unit = {
//    bottomUpVTopDownConfigs = Seq(
//      varPointsToConfig(
//        "souffle-frontend/doop-context-insensitive/database-minijavac",
//        "VarPointsTo",
//        ValueTable.unit(),
//        0,
//        1))
    bottomUpVTopDownConfigs = Seq(
      varPointsToConfig(
        "souffle-frontend/doop-context-insensitive/database-method-call",
        "VarPointsTo",
        ValueTable.unit(),
        0,
        1))
//    val (buTime, buMem) = bottomUpVTopDownConfigs.map(collectBottomUpMeasurements).unzip
    val (buTime, buMem) = (Seq(), Seq())
//    val (tdTime, tdMem) = (Seq(), Seq())
    val (tdTime, tdMem) = bottomUpVTopDownConfigs.map(collectTopDownMeasurements).unzip
    val allTime = buTime ++ tdTime
    val allMem = buMem ++ tdMem
    FilesUtil.writeFile(timeResultsPath, BenchmarkUtils.measurementsToCSV(allTime))
    FilesUtil.writeFile(memResultsPath, BenchmarkUtils.measurementsToCSV(allMem))
  }
}
