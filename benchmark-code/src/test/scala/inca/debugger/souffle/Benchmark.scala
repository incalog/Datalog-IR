package inca.debugger.souffle

import inca.backend.ir.Datalog
import inca.compiler.source.SourceFile
import inca.compiler.CompiledDatalogModule
import inca.compiler.Compiler
import inca.compiler.Options
import inca.debugger.ExternallyInitializableDebugger
import inca.debugger.QueryResult
import inca.debugger.ScalaValue
import inca.debugger.ValueTable
import inca.frontend.constraint.core.ValDef
import inca.frontend.souffle.compiler.CompiledSouffleModule
import inca.frontend.souffle.lowering.SouffleToDatalogIR
import inca.frontend.souffle.lowering.SouffleToNamedRelations
import inca.frontend.souffle.parser.Parser
import inca.measurements.util.BenchmarkUtils
import inca.measurements.util.BenchmarkUtils.Measurement
import inca.measurements.util.BenchmarkUtils.Timing
import inca.measurements.util.Config
import inca.measurements.util.MemoryUtil
import inca.measurements.util.Units
import inca.runtime.context.DataModel
import inca.runtime.context.QueryScope
import inca.runtime.db.DatabaseInput
import inca.runtime.DatalogRuntime
import inca.runtime.EnginePool
import inca.util.FilesUtil
import java.io.File
import org.eclipse.viatra.query.runtime.rete.matcher.DRedReteBackendFactory
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
    val compiled =
      Compiler.compileGP(config.prog, config.dataModel, Options())
    val scope = new QueryScope(config.dataModel)
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
    val expected = debugger.state.readBottomUp(config.entry, config.args)
    debugger.entry(config.entry, config.args)
    val start = System.currentTimeMillis()
    while (!debugger.isFinished) {
      debugger.stepInto()
    }
    val end = System.currentTimeMillis()
    val result = debugger.queryStack.top.asInstanceOf[QueryResult].result

    println(result.size)
    println(result)
    println(expected.size)
    println(expected)
    val tooMuch = result.entries.diff(expected.entries)
    val missing = expected.entries.diff(result.entries)
    println(tooMuch)
    println(missing)
    assert(result == expected)
    MemoryUtil.collectGarbage()
    val mem = MemoryUtil.usedMemoryInBytes()
    println(s"STEPS: ${debugger.irControlTrace.size}")
    println(s"MS/STEP: ${(end - start).toDouble / debugger.irControlTrace.size.toDouble}")
    EnginePool.disposeAllEngines()
    (end - start, mem)
  }

  def collectBottomUpMeasurements(config: BaseConfig): (Measurement, Measurement) = {
    warmup(() => measureBottomUp(config), config)
    val (time, mem) = run(() => measureBottomUp(config), config).unzip
    (
      Measurement("bottomup_time_" + config.name, Units.Milliseconds, time),
      Measurement("bottomup_mem_" + config.name, Units.MegaBytes, mem)
    )
  }

  def collectTopDownMeasurements(config: BaseConfig): (Measurement, Measurement) = {
    warmup(() => measureStepInto(config), config)
    val (time, mem) = run(() => measureStepInto(config), config).unzip
    (
      Measurement("topdown_time_" + config.name, Units.Milliseconds, time),
      Measurement("topdown_mem_" + config.name, Units.MegaBytes, mem)
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

  // step-into produces result in reasonable amount of time
  // find all methods with the name accept and signature java.lang.Object(visitor.GJVisitor,java.lang.Object)
  // will produce 50 tuples complete MethodLookup will have 86005 tuples
  def measureScenario1(): (Measurement, Measurement) = {
    val config = varPointsToConfig(
      "souffle-frontend/benchmark/minijavac",
      "basic_MethodLookup",
      ValueTable(
        Seq("simplename", "descriptor"),
        Seq(
          Seq(
            ScalaValue("accept"),
            ScalaValue("java.lang.Object(visitor.GJVisitor,java.lang.Object)")))),
      0,
      1
    )
    collectTopDownMeasurements(config)
  }

  // step-into produces result in reasonable amount of time
  // find all methods with the name accept
  // will produce 239 tuples complete MethodLookup will have 86005 tuples
  def measureScenario1v2(): (Measurement, Measurement) = {
    val config = varPointsToConfig(
      "souffle-frontend/benchmark/minijavac",
      "basic_MethodLookup",
//      ValueTable(
//        Seq("simplename", "method"),
//        Seq(
//          Seq(
//            ScalaValue("accept"),
//            ScalaValue("<java.net.ServerSocket: java.net.Socket accept()>")))),
      ValueTable(Seq("simplename"), Seq(Seq(ScalaValue("accept")))),
      0,
      1
    )
    collectTopDownMeasurements(config)
  }

  // ground tuple as entry
  def measureScenario1v3(): (Measurement, Measurement) = {
    val config = varPointsToConfig(
      "souffle-frontend/benchmark/minijavac",
      "basic_MethodLookup",
      ValueTable(
        Seq("simplename", "descriptor", "type", "method"),
        Seq(
          Seq(
            ScalaValue("accept"),
            ScalaValue("boolean(java.lang.Object"),
            ScalaValue("java.nio.file.File#1"),
            ScalaValue("<sun.misc.JarFilter: boolean accept(java.io.File,java.lang.String)>")
          ))
      ),
      0,
      1
    )
    collectTopDownMeasurements(config)
  }

  // step-into does not produce result in reasonable amount of time
  // hence, step-over over non-rec pattern is required
  def measureScenario2(): Measurement = {
    ???
    // scenario 2: something that call var points to
    //      varPointsToConfig(
    //        "souffle-frontend/benchmark/minijavac",
    //        "basic_MethodLookup",
    //        ValueTable(
    //          Seq("simplename", "descriptor"),
    //          Seq(
    //            Seq(
    //              ScalaValue("accept"),
    //              ScalaValue("java.lang.Object(visitor.GJVisitor,java.lang.Object)")))),
    //        0,
    //        1
    //      )
  }

  // step-into does not produce result in reasonable amount of time
  // hence, step-over over rec pattern is required
  def measureScenario3(): Measurement = {
    // scenario 3: point var points to for static fields
    ???
  }

  def main(args: Array[String]): Unit = {
    val (sc1tdTime, sc1tdMem) = measureScenario1v2()
    // TODO scenario 2
    // TODO scenario 3
    val allTime = Seq(sc1tdTime)
    val allMem = Seq(sc1tdMem)
    println(sc1tdTime)
    println(sc1tdMem)
    FilesUtil.writeFile(timeResultsPath, BenchmarkUtils.measurementsToCSV(allTime))
    FilesUtil.writeFile(memResultsPath, BenchmarkUtils.measurementsToCSV(allMem))
  }
}
