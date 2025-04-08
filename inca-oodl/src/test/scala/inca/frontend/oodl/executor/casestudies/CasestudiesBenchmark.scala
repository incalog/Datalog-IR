package inca.frontend.oodl.executor.casestudies

import inca.frontend.oodl.compile.{CompiledOODLUnit, OODLCompilerOptions}
import inca.frontend.oodl.executor.OODLExecutor
import inca.frontend.oodl.util.{OODLBenchmark, OODLBenchmarkConfig}
import inca.ir.execution.ThreadCount.Fixed
import inca.ir.optimize
import inca.util.FileUtil
import inca.viatra
import org.eclipse.viatra.query.runtime.rete.matcher.DRedReteBackendFactory

import java.io.File

object CasestudiesBenchmark:
  val outDir: Option[File] = Some(File("benchmark/inca_oodl/CaseStudy"))

  @main
  def measureDependencyAnalysis(): Unit =
    val endNode = 100
    val step = 10

    val code = FileUtil.readFileFromResource("objectoriented/casestudies/DependencyAnalysis.oodl")
    val options = OODLCompilerOptions.default

    // Measure statistics exactly once
    val statConfig = OODLBenchmarkConfig("", "", code, OODLExecutor(viatra.backend.Executor()), options,
      optimizationPipeline = CompiledOODLUnit.optimizationPipeline
    )
    val statBenchmark = OODLBenchmark("DependencyAnalysis", Seq(statConfig), "main", Seq(endNode, step), outDir)

    val statsDs = statBenchmark.measureStatistics()
    val optimDs = statBenchmark.measureOptimizations()
    val sizeDs = statBenchmark.measureRelationStatistics()

    println(statsDs.toTable)
    println(sizeDs.toTable)
    println(optimDs.toTable)

    // Measure execution time
    val execs = Seq(viatra.backend.Executor())
    val configs = execs.flatMap { exec =>
      Seq(
        OODLBenchmarkConfig(exec.name, "unoptimized", code, OODLExecutor(exec), options),
        OODLBenchmarkConfig(exec.name, "optimized", code, OODLExecutor(exec), options, optimizationPipeline = CompiledOODLUnit.optimizationPipeline)
      )
    }
    val benchmark = OODLBenchmark("DependencyAnalysis", configs, "main", Seq(endNode, step), outDir)
    val (perfDs, _) = benchmark.measureAndPlotPerformance(runs = 10, warmups = 5, xLabel = Some("Engine"))
    println(perfDs.toTable)

  @main
  def measureDependencyAnalysisIntraVsInter(): Unit =
    def interRelationalOptimizationPipeline = List(
      () => new optimize.RemoveDuplicatedRelations {},
      () => new optimize.IRConstantOptimizer(computeControlEvents = false, interRelational = false) {},
      () => new optimize.IdentityCastElimination {},
      () => new optimize.IRConstantOptimizer(computeControlEvents = false, interRelational = true) {},
      () => new optimize.IdentityCastElimination {},
      () => new optimize.ReplaceSingletonVariables {}, // helps with detecting exact duplicates
      () => new optimize.RemoveDuplicatedRelations {},
      () => new optimize.AliasElimination {}
    )

    def intraRelationalOptimizationPipeline = List(
      () => new optimize.RemoveDuplicatedRelations {},
      () => new optimize.IRConstantOptimizer(computeControlEvents = false, interRelational = false) {},
      () => new optimize.IdentityCastElimination {},
      () => new optimize.ReplaceSingletonVariables {}, // helps with detecting exact duplicates
      () => new optimize.RemoveDuplicatedRelations {},
      () => new optimize.AliasElimination {}
    )

    val endNode = 100
    val step = 10

    val code = FileUtil.readFileFromResource("objectoriented/casestudies/DependencyAnalysis.oodl")
    val options = OODLCompilerOptions.default

    // Measure statistics exactly once
    val statConfigs = Seq(
      OODLBenchmarkConfig("intra", "", code, OODLExecutor(viatra.backend.Executor()), options, optimizationPipeline = intraRelationalOptimizationPipeline),
      OODLBenchmarkConfig("inter", "", code, OODLExecutor(viatra.backend.Executor()), options, optimizationPipeline = interRelationalOptimizationPipeline)
    )
    val statBenchmark = OODLBenchmark("DependencyAnalysis", statConfigs, "main", Seq(endNode, step), outDir)

    val statsDs = statBenchmark.measureStatistics()
    val optimDs = statBenchmark.measureOptimizations()
    val sizeDs = statBenchmark.measureRelationStatistics()

    println(statsDs.toTable)
    println(sizeDs.toTable)
    println(optimDs.toTable)

  @main
  def measureControlFlow(): Unit =
    val code = FileUtil.readFileFromResource("objectoriented/casestudies/CfgVisitor.oodl")
    val options = OODLCompilerOptions.default

    // Measure statistics exactly once
    val statConfig = OODLBenchmarkConfig("", "", code, OODLExecutor(viatra.backend.Executor()), options,
      optimizationPipeline = CompiledOODLUnit.optimizationPipeline
    )
    val statBenchmark = OODLBenchmark("CfgVisitor", Seq(statConfig), "main", Seq(), outDir)

    val statsDs = statBenchmark.measureStatistics()
    val optimDs = statBenchmark.measureOptimizations()
    val sizeDs = statBenchmark.measureRelationStatistics()

    println(statsDs.toTable)
    println(sizeDs.toTable)
    println(optimDs.toTable)

    // Measure execution time
    val execs = Seq(viatra.backend.Executor())
    val configs = execs.flatMap { exec =>
      Seq(
        OODLBenchmarkConfig(exec.name, "unoptimized", code, OODLExecutor(exec), options),
        OODLBenchmarkConfig(exec.name, "optimized", code, OODLExecutor(exec), options, optimizationPipeline = CompiledOODLUnit.optimizationPipeline)
      )
    }
    val benchmark = OODLBenchmark("CfgVisitor", configs, "main", Seq(), outDir)
    val (perfDs, _) = benchmark.measureAndPlotPerformance(runs = 10, warmups = 5, xLabel = Some("Engine"))
    println(perfDs.toTable)

  @main
  def measureControlFlowIntraVsInter(): Unit =
    def interRelationalOptimizationPipeline = List(
      () => new optimize.RemoveDuplicatedRelations {},
      () => new optimize.IRConstantOptimizer(computeControlEvents = false, interRelational = false) {},
      () => new optimize.IdentityCastElimination {},
      () => new optimize.IRConstantOptimizer(computeControlEvents = false, interRelational = true) {},
      () => new optimize.IdentityCastElimination {},
      () => new optimize.ReplaceSingletonVariables {}, // helps with detecting exact duplicates
      () => new optimize.RemoveDuplicatedRelations {},
      () => new optimize.AliasElimination {}
    )

    def intraRelationalOptimizationPipeline = List(
      () => new optimize.RemoveDuplicatedRelations {},
      () => new optimize.IRConstantOptimizer(computeControlEvents = false, interRelational = false) {},
      () => new optimize.IdentityCastElimination {},
      () => new optimize.ReplaceSingletonVariables {}, // helps with detecting exact duplicates
      () => new optimize.RemoveDuplicatedRelations {},
      () => new optimize.AliasElimination {}
    )

    val code = FileUtil.readFileFromResource("objectoriented/casestudies/CfgVisitor.oodl")
    val options = OODLCompilerOptions.default
    //options.irLogging.logTypeInformation = true
    //options.irLogging.logOptimizations = true

    // Measure statistics exactly once
    val statConfigs = Seq(
      OODLBenchmarkConfig("intra", "", code, OODLExecutor(viatra.backend.Executor()), options, optimizationPipeline = intraRelationalOptimizationPipeline),
      OODLBenchmarkConfig("inter", "", code, OODLExecutor(viatra.backend.Executor()), options, optimizationPipeline = interRelationalOptimizationPipeline)
    )
    val statBenchmark = OODLBenchmark("CfgVisitor", statConfigs, "main", Seq(), outDir )

    val statsDs = statBenchmark.measureStatistics()
    val optimDs = statBenchmark.measureOptimizations()
    val sizeDs = statBenchmark.measureRelationStatistics()

    println(statsDs.toTable)
    println(sizeDs.toTable)
    println(optimDs.toTable)

  @main
  def measureSignAnalysis(): Unit =
    val code = FileUtil.readFileFromResource("objectoriented/casestudies/FlowSensitiveSignAnalysis.oodl")
    val options = OODLCompilerOptions.default

    // Measure statistics exactly once
    val statConfig = OODLBenchmarkConfig("", "", code, OODLExecutor(viatra.backend.Executor(DRedReteBackendFactory.INSTANCE)), options,
      optimizationPipeline = CompiledOODLUnit.optimizationPipeline,
      includePostProcessingPipeline = true
    )
    val statBenchmark = OODLBenchmark("FlowSensitiveSignAnalysis", Seq(statConfig), "main", Seq(), outDir)

    val statsDs = statBenchmark.measureStatistics()
    val optimDs = statBenchmark.measureOptimizations()
    val sizeDs = statBenchmark.measureRelationStatistics()

    println(statsDs.toTable)
    println(sizeDs.toTable)
    println(optimDs.toTable)

    // Measure execution time
    val execs = Seq(viatra.backend.Executor(DRedReteBackendFactory.INSTANCE))
    val configs = execs.flatMap { exec =>
      Seq(
        OODLBenchmarkConfig(exec.name, "unoptimized", code, OODLExecutor(exec), options, includePostProcessingPipeline = true),
        OODLBenchmarkConfig(exec.name, "optimized", code, OODLExecutor(exec), options, optimizationPipeline = CompiledOODLUnit.optimizationPipeline, includePostProcessingPipeline = true)
      )
    }
    val benchmark = OODLBenchmark("FlowSensitiveSignAnalysis", configs, "main", Seq(), outDir)
    val (perfDs, _) = benchmark.measureAndPlotPerformance(runs = 10, warmups = 5, xLabel = Some("Engine"))
    println(perfDs.toTable)

  @main
  def measureSignAnalysisDemandStrategies(): Unit =
    val code = FileUtil.readFileFromResource("objectoriented/casestudies/FlowSensitiveSignAnalysis.oodl")
    val options = OODLCompilerOptions.default

    // Measure execution time
    val execs = Seq(viatra.backend.Executor(DRedReteBackendFactory.INSTANCE))
    val configs = execs.flatMap { exec =>
      Seq(
        OODLBenchmarkConfig(exec.name, "normal", code, OODLExecutor(exec), options, pipeline = CompiledOODLUnit.createPipeline(false), optimizationPipeline = CompiledOODLUnit.optimizationPipeline, includePostProcessingPipeline = true),
        OODLBenchmarkConfig(exec.name, "supplementary", code, OODLExecutor(exec), options, pipeline = CompiledOODLUnit.createPipeline(true), optimizationPipeline = CompiledOODLUnit.optimizationPipeline, includePostProcessingPipeline = true)
      )
    }
    val benchmark = OODLBenchmark("FlowSensitiveSignAnalysis_demand", configs, "main", Seq(), outDir)
    val statsDs = benchmark.measureStatistics()
    val optimDs = benchmark.measureOptimizations()
    val sizeDs = benchmark.measureRelationStatistics()
    val (perfDs, _) = benchmark.measureAndPlotPerformance(runs = 10, warmups = 5, xLabel = Some("Engine"))

    println(statsDs.toTable)
    println(sizeDs.toTable)
    println(optimDs.toTable)
    println(perfDs.toTable)