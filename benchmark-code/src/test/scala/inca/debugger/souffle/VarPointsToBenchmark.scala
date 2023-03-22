package inca.debugger.souffle

import inca.compiler.CompiledDatalogModule
import inca.compiler.Compiler
import inca.compiler.Options
import inca.debugger.souffle.Configs.scenario1v2
import inca.debugger.souffle.Configs.scenario2v1
import inca.debugger.souffle.Configs.scenario3Orcale1
import inca.debugger.souffle.Configs.scenario3Orcale2
import inca.debugger.souffle.Configs.scenario3Orcale3
import inca.debugger.souffle.Configs.scenario3Orcale4
import inca.debugger.souffle.Configs.scenario3v1
import inca.debugger.souffle.Configs.BaseConfig
import inca.debugger.souffle.Configs.DebuggingSemantics
import inca.debugger.souffle.Configs.DebuggingSemantics.HybridSemantics
import inca.debugger.souffle.Configs.DoopProgram
import inca.debugger.souffle.Configs.Oracle
import inca.debugger.BlacklistTransformation
import inca.debugger.ExternallyInitializableDebugger
import inca.debugger.Predicate
import inca.debugger.Query
import inca.debugger.Query.toTableless
import inca.debugger.QueryResult
import inca.debugger.QueryState
import inca.measurements.util.BenchmarkUtils
import inca.measurements.util.BenchmarkUtils.Timing
import inca.measurements.util.CSVUtil.csvToString
import inca.measurements.util.CSVUtil.CSV
import inca.measurements.util.CSVUtil.CSVRow
import inca.measurements.util.MemoryUtil
import inca.runtime.context.QueryScope
import inca.runtime.DatalogRuntime
import inca.runtime.EnginePool
import inca.util.FilesUtil
import org.eclipse.viatra.query.runtime.rete.matcher.TimelyReteBackendFactory
import scala.collection.mutable

object VarPointsToBenchmark {
  val resultsPath: String = "benchmark-results/debugger/data"

  def initRuntime(
      config: BaseConfig
    ): (CompiledDatalogModule, CompiledDatalogModule, DatalogRuntime) = {
    val blModule = BlacklistTransformation.transformer(config.compiled.dataModel).transformModule(
      config.compiled.ir)
    val compiled =
      Compiler.compileGP(config.compiled.ir, config.compiled.dataModel, Options())
    val blCompiled = Compiler.compileGP(blModule, config.compiled.dataModel, Options())
    val scope = new QueryScope(config.compiled.dataModel)
    val (_engine, _database) =
      EnginePool.loadEngineAndDatabase(scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
    // EnginePool.loadEngineAndDatabase(scope, DRedReteBackendFactory.INSTANCE)
    (blCompiled, compiled, DatalogRuntime(_engine, _database, blCompiled))
  }

  // returns running time and memory
  //  def measureBottomUp(config: BaseConfig): (Long, Long) = {
  //    val (compiled, runtime) = initRuntime(config)
  //    // measure running time
  //    val matcher = runtime.engine.getMatcher(compiled.psystemModule.patterns(config.entry)())
  //    val start = System.currentTimeMillis()
  //    runtime.engine.delayUpdatePropagation { () =>
  //      runtime.db.processDatabaseInput(config.input)
  //    }
  //    val end = System.currentTimeMillis()
  //    val endCount = System.currentTimeMillis()
  //    // measure memory
  //    MemoryUtil.collectGarbage()
  //    val mem = MemoryUtil.usedMemoryInMBytes()
  //
  //    EnginePool.disposeAllEngines()
  //    (end - start, mem)
  //  }

  def initBottomUp(
      compiled: CompiledDatalogModule,
      runtime: DatalogRuntime,
      config: BaseConfig
    ): Unit = {
    runtime.engine.delayUpdatePropagation { () =>
      runtime.db.processDatabaseInput(config.input)
    }
    val matcher = runtime.engine.getMatcher(compiled.psystemModule.patterns("VarPointsTo")())
    println("VarPointsTo Size: ")
    println(matcher.countMatches())
  }

//  def measureStepInto(config: BaseConfig): (Seq[Long], Long, Long) = {
//    val (blModule, ogModule, runtime) = initRuntime(config)
//    val debugger =
//      new ExternallyInitializableDebugger(ogModule, config.semantics.debuggingState)
//    // initialize bottom-up database
//    initBottomUp(blModule, runtime, config)
//    debugger.setRuntime(runtime)
//    println("START MEASUREMENT")
//    debugger.entry(config.entry, config.args)
//    val measurements: mutable.ListBuffer[Long] = mutable.ListBuffer()
//    while (!debugger.isFinished) {
//      val start = System.nanoTime()
//      debugger.stepInto()
//      val end = System.nanoTime()
//      // println(debugger.queryStack.top)
//      measurements += end - start
//    }
//
//    val result = debugger.queryStack.top.asInstanceOf[QueryResult].result
//    val expected = debugger.state.readBottomUp(config.entry, config.args)
//
//    println(result.size)
//    println(result)
//    println(expected.size)
//    println(expected)
//    val tooMuch = result.entries.diff(expected.entries)
//    val missing = expected.entries.diff(result.entries)
//    println(tooMuch)
//    println(missing)
//    assert(result == expected)
//
//    MemoryUtil.collectGarbage()
//    val mem = MemoryUtil.usedMemoryInMBytes()
//    //    println(s"STEPS: ${debugger.irControlTrace.size}")
//    //    println(s"MS/STEP: ${(end - start).toDouble / debugger.irControlTrace.size.toDouble}")
//    EnginePool.disposeAllEngines()
//    (measurements.toSeq, mem, debugger.irControlTrace.size - 1)
//  }

//  def measureStepOut(config: BaseConfig): (Seq[Long], Long, Long) = {
////    val (module, runtime) = initRuntime(config)
////    val debugger = new ExternallyInitializableDebugger(module, config.semantics.debuggingState)
////    // initialize bottom-up database
////    initBottomUp(module, runtime, config)
//    val (blModule, ogModule, runtime) = initRuntime(config)
//    val debugger =
//      new ExternallyInitializableDebugger(ogModule, config.semantics.debuggingState)
//    // initialize bottom-up database
//    initBottomUp(blModule, runtime, config)
//    debugger.setRuntime(runtime)
//    println("START MEASUREMENT")
//    debugger.entry(config.entry, config.args)
//    val start = System.currentTimeMillis()
//    debugger.stepOut()
//    val end = System.currentTimeMillis()
//
//    val expected = debugger.state.readBottomUp(config.entry, config.args)
//    val result = debugger.queryStack.top.asInstanceOf[QueryResult].result
//    println(result.size)
//    println(result)
//    println(expected.size)
//    println(expected)
//    val tooMuch = result.entries.diff(expected.entries)
//    val missing = expected.entries.diff(result.entries)
////    println(tooMuch)
////    println(missing)
//    assert(result == expected)
//
//    MemoryUtil.collectGarbage()
//    val mem = MemoryUtil.usedMemoryInMBytes()
//    EnginePool.disposeAllEngines()
//    (Seq(end - start), mem, debugger.irControlTrace.size - 1)
//  }

  def measureInteractive(
      config: BaseConfig,
      oracle: Oracle,
      timeout: Option[Long] = None
    ): (Seq[Long], Map[Predicate, Seq[Long]], Long) = {
    val (blModule, ogModule, runtime) = initRuntime(config)
    val debugger = new ExternallyInitializableDebugger(ogModule, config.semantics.debuggingState)
    // initialize bottom-up database
    initBottomUp(blModule, runtime, config)
    debugger.setRuntime(runtime)
    debugger.entry(config.entry, config.args)
    val intoMeasurements: mutable.ListBuffer[Long] = mutable.ListBuffer()
    val overMeasurements: mutable.Map[String, Seq[Long]] = mutable.Map()
    def extendOver(pred: Predicate, time: Long): Unit = {
      overMeasurements.get(pred) match {
        case Some(value) =>
          overMeasurements(pred) = value ++ Seq(time)
        case None =>
          overMeasurements(pred) = Seq(time)
      }
    }
    def timeoutReached(vals: Map[String, Seq[Long]]): Boolean = {
      timeout match {
        case Some(till) =>
          val reached = till - vals.flatMap(_._2).sum < 0
          if (reached) {
            println(s"${config.name} TIMEOUTREACHED(${timeout.getOrElse(0L)}) = $reached")
          }
          reached
        case None => false
      }
    }
    while (
      !timeoutReached(
        overMeasurements.toMap + ("IntoMeasurement" -> intoMeasurements.toSeq)) && !debugger.isFinished
    ) {
      val top = debugger.queryStack.top
      val (into, callee) = oracle.shouldStepInto(top)
      if (into) {
        val start = System.nanoTime()
        debugger.stepInto()
        val end = System.nanoTime()
        intoMeasurements += end - start
      } else {
        val start = System.nanoTime()
        debugger.stepOver()
        val end = System.nanoTime()
        extendOver(callee, end - start)
      }
    }

//    val expected = debugger.state.readBottomUp(config.entry, config.args)
//    val result = debugger.queryStack.top.asInstanceOf[QueryResult].result
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
    MemoryUtil.collectGarbage()
    (intoMeasurements.toSeq, overMeasurements.toMap, mem)
  }

  def collectMeasurements(
      config: BaseConfig,
      f: BaseConfig => (Seq[Long], Long, Long)
    ): CSVRow = {
    val (time, mem, steps) = f(config)
    IndexedSeq[Any](steps, mem) ++ time
  }

  def addCSVHeader(rows: CSV): CSV = {
    val numberOfStepsInto = rows.head(0).asInstanceOf[Long].toInt
    val stepColumns = (1 to numberOfStepsInto).map(i => s"step$i")
    val columnHeader: CSVRow = IndexedSeq("NumberOfSteps", "Memory (MB)") ++ stepColumns
    columnHeader +: rows
  }

  def measureAndWrite(
      config: BaseConfig,
      f: BaseConfig => (Seq[Long], Long, Long),
      filePostFix: String = "StepInto"
    ): Unit = {
    val measurements: CSV =
      BenchmarkUtils.measure(() => collectMeasurements(config, f), config).toIndexedSeq
    val csv = addCSVHeader(measurements)
    FilesUtil.writeFile(s"$resultsPath/${config.name}_${filePostFix}.csv", csvToString(csv))
  }

  def measureInteractiveAndWrite(
      config: BaseConfig,
      oracle: Oracle
    ): Unit = {
    val measurements = BenchmarkUtils.measure(() => measureInteractive(config, oracle), config)
    measurements.foreach { case (into, over, mem) =>
      println("RUN")
      println(into.size)
      val intoMs = into.map(_.toDouble / 1000000)
      // TODO how to store the data in csv?
      // What data do we want to store?
      // What data do we have?
      // NumberOfStepIntos (All steps that trigger a step into)
      // measurement of step into execution (individually)
      // memory after run
      // NumberOfStepOvers for a predicate
      // measurement of step over execution (individually)
      println(intoMs)
      println(intoMs.sum)
      over.foreach { case (p, vs) =>
        println(p)
        println(s"  size ${vs.size}")
        println("  " + vs.map(_.toDouble / 1000000).mkString(", "))
      }
      // println(into.size + over.map(_._2.size).sum)
      println(mem)
      val total = intoMs.sum + over.map(_._2.map(_.toDouble / 1000000).sum).sum
      println(s"TOTALTIME: $total")
    }
    //    // intoSteps, memory, OverStepsX, OverStepsY, OverStepsZ,
    //    val numIntoSteps = measurements.head._1.size
    //    val numOverPred = measurements.head._2.keys.size
    //    val overPredHeader = measurements.head._2.keys.toSeq.map("NumberOfOverSteps" + _)
    //    val intoStepsHeader = (1 to numIntoSteps).map("intoStep" + _)
    //    val columnHeader = IndexedSeq("NumberOfIntoSteps", "Memory (MB)", "NumberOfOverPreds") ++ intoStepsHeader ++
    //    val rows = measurements.map { case (intoSteps, overSteps, mem) =>
    //      (intoSteps.)
    //    }
    //    columnHeader +: rows
    //
  }

  def measureEachSemanticsRule(
      config: BaseConfig,
      timeout: Option[Long] = None
    ): Map[String, Seq[Long]] = {
    val (blModule, ogModule, runtime) = initRuntime(config)
    val debugger = new ExternallyInitializableDebugger(ogModule, config.semantics.debuggingState)
    // initialize bottom-up database
    initBottomUp(blModule, runtime, config)
    debugger.setRuntime(runtime)
    debugger.entry(config.entry, config.args)

    // rule measurement util
    val ruleMeasurements: mutable.Map[String, Seq[Long]] = mutable.Map()
    def extendRuleSemantics(rule: String, time: Long): Unit = {
      ruleMeasurements.get(rule) match {
        case Some(value) =>
          ruleMeasurements(rule) = value ++ Seq(time)
        case None =>
          ruleMeasurements(rule) = Seq(time)
      }
    }

    def timeoutReached(vals: Map[String, Seq[Long]]): Boolean = {
      timeout match {
        case Some(till) =>
          till - vals.flatMap(_._2).sum < 0
        case None => false
      }
    }

    while (!timeoutReached(ruleMeasurements.toMap) && !debugger.isFinished) {
      // prepare to decide which rule will be measured
      val top = debugger.queryStack.top
      var ruleName: String = ""
      var stackHeight: Option[Int] = None
      var queryStableOrIterate = false
      top.state match {
        case QueryState.AtomIntoOrSkip => stackHeight = Some(debugger.queryStack.size)
        case QueryState.QueryEnd => queryStableOrIterate = true
        case s => ruleName = s.toString
      }

      // measure
      val start = System.nanoTime()
      debugger.stepInto()
      val end = System.nanoTime()

      // decide which rule will be measured finally (Skip vs Into, Stable vs Iterate)
      if (stackHeight.nonEmpty) {
        if (debugger.queryStack.size > stackHeight.get) {
          ruleName = "AtomInto"
        } else {
          ruleName = "AtomSkip"
        }
      }
      if (queryStableOrIterate) {
        if (debugger.queryStack.top.isInstanceOf[QueryResult]) {
          ruleName = "QueryStable"
        } else {
          ruleName = "QueryIterate"
        }
      }
      extendRuleSemantics(ruleName, end - start)
    }

//    val result = debugger.queryStack.top.asInstanceOf[QueryResult].result
//    val expected = debugger.state.readBottomUp(config.entry, config.args)
//
//    println(result.size)
//    println(result)
//    println(expected.size)
//    println(expected)
//    val tooMuch = result.entries.diff(expected.entries)
//    val missing = expected.entries.diff(result.entries)
//    println(tooMuch)
//    println(missing)
//    assert(result == expected)

//    MemoryUtil.collectGarbage()
//    val mem = MemoryUtil.usedMemoryInMBytes()
//    println(s"ATOMINTOS ${debugger.numerOfAtomInto}")
//    println(s"RECORDED INTOS: ${ruleMeasurements("AtomInto").size}")
    EnginePool.disposeAllEngines()
    MemoryUtil.collectGarbage()
    ruleMeasurements.toMap
  }

  def main(args: Array[String]): Unit = {
    val warmupInNano: Long = 2L * 60L * 1000L * 1000000L
    val tenMinutesInNano: Long = 10L * 60L * 1000L * 1000000L
    val intoConfigs = Seq(
      scenario1v2(DoopProgram.MiniJavac, DebuggingSemantics.PureIntoSemantics),
      scenario2v1(DoopProgram.MiniJavac, DebuggingSemantics.PureIntoSemantics),
      scenario3v1(DoopProgram.MiniJavac, DebuggingSemantics.PureIntoSemantics)
      // scenario1v2(DoopProgram.MiniJavac, DebuggingSemantics.HybridOptSemantics),
      // scenario2v1(DoopProgram.MiniJavac, DebuggingSemantics.HybridOptSemantics),
      // scenario3v1(DoopProgram.MiniJavac, DebuggingSemantics.HybridOptSemantics)
    )
    val overConfigs = Seq(
      scenario3v1(DoopProgram.MiniJavac, DebuggingSemantics.HybridSemantics) -> scenario3Orcale1,
      scenario3v1(DoopProgram.MiniJavac, DebuggingSemantics.HybridSemantics) -> scenario3Orcale2,
      scenario3v1(DoopProgram.MiniJavac, DebuggingSemantics.HybridSemantics) -> scenario3Orcale3,
      scenario3v1(DoopProgram.MiniJavac, DebuggingSemantics.HybridSemantics) -> scenario3Orcale4
      // scenario3v1(DoopProgram.MiniJavac, DebuggingSemantics.HybridOptSemantics) -> scenario3Orcale1,
      // scenario3v1(DoopProgram.MiniJavac, DebuggingSemantics.HybridOptSemantics) -> scenario3Orcale2,
      // scenario3v1(DoopProgram.MiniJavac, DebuggingSemantics.HybridOptSemantics) -> scenario3Orcale3,
      // scenario3v1(DoopProgram.MiniJavac, DebuggingSemantics.HybridOptSemantics) -> scenario3Orcale4
    )

    println("INTO")
    for (c <- intoConfigs) {
      for (i <- 0 until c.warmup) {
        println("WARMUP")
        measureEachSemanticsRule(c, Some(warmupInNano))
      }
      println("RUN")
      val measurements = measureEachSemanticsRule(c, Some(tenMinutesInNano))
      measurements.foreach { case (rule, vals) =>
        val rows = vals.map(v => IndexedSeq(v)).toIndexedSeq
        val csv = IndexedSeq("measurement") +: rows
        FilesUtil.writeFile(s"$resultsPath/${c.name}_$rule.csv", csvToString(csv))
      }

      val all = measurements.flatMap(_._2).toIndexedSeq
      val allRows = all.map(v => IndexedSeq(v))
      val allCSV = IndexedSeq("measurement") +: allRows
      FilesUtil.writeFile(s"$resultsPath/${c.name}_ALL.csv", csvToString(allCSV))
    }

    println("OVER")
    for ((c, o) <- overConfigs) {
      for (i <- 0 until c.warmup) {
        // do warmup
        println("WARMUP")
        measureInteractive(c, o, Some(warmupInNano))
      }
      println("RUN")
      val (into, over, mem) = measureInteractive(c, o, Some(tenMinutesInNano))
      println(s"${c.name} with ${o.name} memory ${mem}")
      println(s"${c.name} with ${o.name} intotime ${into.sum}")
      val all = over.flatMap(_._2).toIndexedSeq
      val allRows = all.map(v => IndexedSeq(v))
      val allCSV = IndexedSeq("measurement") +: allRows
      FilesUtil.writeFile(s"$resultsPath/${c.name}_${o.name}_Over.csv", csvToString(allCSV))
    }
  }
}
