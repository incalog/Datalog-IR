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
import inca.measurements.util.BenchmarkUtils.measure
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
import scala.collection.mutable

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
    def name: String = s"${prog.name}_${entry}_${args.columns.mkString(";")}"
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

  val resultsPath: String = "benchmark-results/debugger/"

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

  def measureStepInto(config: BaseConfig): (Seq[Long], Long, Long, Option[Long]) = {
    val (module, runtime) = initRuntime(config)
    val debugger = new ExternallyInitializableDebugger(module)
    // initialize bottom-up database
    runtime.engine.delayUpdatePropagation { () =>
      runtime.db.processDatabaseInput(config.input)
    }
    debugger.setRuntime(runtime)
    // val expected = debugger.state.readBottomUp(config.entry, config.args)
    debugger.entry(config.entry, config.args)
    val measurements: mutable.ListBuffer[Long] = mutable.ListBuffer()
    while (!debugger.isFinished) {
      val start = System.nanoTime()
      debugger.stepInto()
      val end = System.nanoTime()
      measurements += end - start
    }

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
    val mem = MemoryUtil.usedMemoryInBytes()
//    println(s"STEPS: ${debugger.irControlTrace.size}")
//    println(s"MS/STEP: ${(end - start).toDouble / debugger.irControlTrace.size.toDouble}")
    EnginePool.disposeAllEngines()
    (measurements.toSeq, mem, debugger.irControlTrace.size, None)
  }

  def measureStepOut(config: BaseConfig): (Seq[Long], Long, Long, Option[Long]) = {
    val (module, runtime) = initRuntime(config)
    val debugger = new ExternallyInitializableDebugger(module)
    // initialize bottom-up database
    runtime.engine.delayUpdatePropagation { () =>
      runtime.db.processDatabaseInput(config.input)
    }
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
    val mem = MemoryUtil.usedMemoryInBytes()
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

  def souffleVarPointsToConfig(
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

  def baseConfig(entry: String, t: ValueTable): BaseConfig =
    souffleVarPointsToConfig("souffle-frontend/benchmark/minijavac", entry, t, 0, 1)

  // scenario 1
  def methodLookupConfig(t: ValueTable): BaseConfig =
    baseConfig("basic_MethodLookup", t)

  // scenario 2
  def varPointsToConfig(t: ValueTable): BaseConfig =
    baseConfig("basic_MethodLookup", t)

  // step-into produces result in reasonable amount of time
  // find all methods with the name accept and signature java.lang.Object(visitor.GJVisitor,java.lang.Object)
  // will produce 50 tuples complete MethodLookup will have 86005 tuples
  def measureScenario1(): (Seq[Measurement], Seq[Measurement]) = {
    val config = methodLookupConfig(
      ValueTable(
        Seq("simplename", "descriptor"),
        Seq(
          Seq(
            ScalaValue("accept"),
            ScalaValue("java.lang.Object(visitor.GJVisitor,java.lang.Object)"))))
    )
    measureIntoAndOver(config)
  }

  // step-into produces result in reasonable amount of time
  // find all methods with the name accept
  // will produce 239 tuples complete MethodLookup will have 86005 tuples

  def measureIntoAndOver(config: BaseConfig): (Seq[Measurement], Seq[Measurement]) = {
    val intoMeasurements =
      BenchmarkUtils.measure(() => collectMeasurements(config, measureStepInto), config)
    val overMeasurements =
      BenchmarkUtils.measure(() => collectMeasurements(config, measureStepOut), config)
    (intoMeasurements, overMeasurements)
  }
  def measureScenario1v2(): (Seq[Measurement], Seq[Measurement]) = {
    val config = methodLookupConfig(ValueTable(Seq("simplename"), Seq(Seq(ScalaValue("accept")))))
    measureIntoAndOver(config)
  }

  // ground tuple as entry
  def measureScenario1v3(): (Seq[Measurement], Seq[Measurement]) = {
    val config = methodLookupConfig(
      ValueTable(
        Seq("simplename", "descriptor", "type", "method"),
        Seq(Seq(
          ScalaValue("accept"),
          ScalaValue("boolean(java.lang.Object"),
          ScalaValue("java.nio.file.File#1"),
          ScalaValue("<sun.misc.JarFilter: boolean accept(java.io.File,java.lang.String)>")
        ))
      ))
    measureIntoAndOver(config)
  }

  // step-into does not produce result in reasonable amount of time
  // hence, step-over over non-rec pattern is required
  def measureScenario2(): (Seq[Measurement], Seq[Measurement]) = {
    // scenario 2: something that call var points to
    val config = varPointsToConfig(
      ValueTable(
        Seq("var", "heap"),
        Seq(
          Seq(
            ScalaValue(
              "<typechecking.ClassSymbol: int putMethod(java.lang.String,typechecking.MethodSymbol)>/$r1"),
            ScalaValue("<<HASH:1808431609>>")))
      )
    )
    measureIntoAndOver(config)
  }
  def measureScenario2v2(): (Seq[Measurement], Seq[Measurement]) = {
    // scenario 2: something that call var points to
    val config = varPointsToConfig(
      ValueTable(
        Seq("var"),
        Seq(Seq(ScalaValue(
          "<typechecking.ClassSymbol: int putMethod(java.lang.String,typechecking.MethodSymbol)>/$r1"))))
    )
    measureIntoAndOver(config)
  }

  // step-into does not produce result in reasonable amount of time
  // hence, step-over over rec pattern is required
  def measureScenario3(): (Seq[Measurement], Seq[Measurement]) = {
    // scenario 3: point var points to for static fields
    ???
  }

  def main(args: Array[String]): Unit = {
    val (sc1Into, sc1Over) = measureScenario1()
    // TODO scenario 2
    // TODO scenario 3
    FilesUtil.writeFile(
      s"$resultsPath/${sc1Into.head.name}-StepInto.csv",
      BenchmarkUtils.measurementsToCSV(sc1Into))
    FilesUtil.writeFile(
      s"$resultsPath/${sc1Over.head.name}-StepOver.csv",
      BenchmarkUtils.measurementsToCSV(sc1Over))
  }
}
