package inca.util

import benchmark.plot.{BarConfig, Plotter}
import benchmark.plot.option.Color.Blue
import benchmark.plot.option.Color
import benchmark.plot.option.builder.BarplotOptionsBuilder
import benchmark.util
import benchmark.util.{Dataset, DoubleValue, IntValue, LongValue, StringValue, TimeUnit}
import inca.ir.CompiledUnit
import inca.ir.execution.{ExecutorEngine, Relation}
import inca.ir.visitors.StatisticsCollector

import java.awt.Desktop
import java.io.{File, IOException}

def collectGarbage(): Unit = {
  System.gc()
  try {
    Thread.sleep(2000)
  } catch {
    case e: IOException => e.printStackTrace()
  }
}

trait BenchmarkConfig:
  val name: String

// TODO: Introduce grouped box plots
trait Benchmark[Config <: BenchmarkConfig, CUnit <: CompiledUnit]:
  val name: String
  val configs: Seq[Config]
  val outDir: Option[File]

  def storeFiles: Boolean = outDir.isDefined

  def getOutFile(fileName: String): File =
    outDir match
      case Some(dir) => File(s"${dir.getAbsolutePath}/$fileName")
      case _ => File.createTempFile("", fileName)

  def setupEngine(config: Config): ExecutorEngine

  def setupCompiledUnit(config: Config): CUnit

  def measureRelationStatistics(): Dataset =
    val rows = configs.flatMap { config =>
      val engine = setupEngine(config)
      val outputRels = engine.readAll()
      val configValue = StringValue(config.name)
      val totalTuples = IndexedSeq(configValue, StringValue("Total"), IntValue(outputRels.map(_.size).sum))
      val tuplesPerRelation = outputRels.map { r => IndexedSeq(configValue, StringValue(r.name), IntValue(r.size)) }
      totalTuples +: tuplesPerRelation
    }
    val ds = Dataset("Relations", IndexedSeq("Config", "Name", "Amount"), rows)
    if (storeFiles) FileUtil.writeFile(getOutFile(s"${name}_relations_stats.csv"), ds.toCSV())
    ds

  def measureStatistics(): Dataset =
    val rows = configs.flatMap { config =>
      val compiled = setupCompiledUnit(config)
      val configValue = StringValue(config.name)
      val statsBeforeLowering = StatisticsCollector.collect(compiled.irModules.head)
      val statsAfterLowering = StatisticsCollector.collect(compiled.lowered.head)
      val statsAfterOptimization = StatisticsCollector.collect(compiled.optimized.head)
      val rowsInitial = statsBeforeLowering.map { case (k, v) =>
        IndexedSeq(configValue, StringValue("initial"), StringValue(k), IntValue(v))
      }
      val rowsLowered = statsAfterLowering.map { case (k, v) =>
        IndexedSeq(configValue, StringValue("lowered"), StringValue(k), IntValue(v))
      }
      val rowsOptimized = statsAfterOptimization.map { case (k, v) =>
        IndexedSeq(configValue, StringValue("optimized"), StringValue(k), IntValue(v))
      }
      rowsInitial ++ rowsLowered ++ rowsOptimized
    }
    val ds = Dataset("Statistics", IndexedSeq("Config", "Name", "Kind", "Value"), rows)
    if (storeFiles) FileUtil.writeFile(getOutFile(s"${name}_stats.csv"), ds.toCSV())
    ds

  def measureOptimizations(): Dataset =
    val rows = configs.flatMap { config =>
      val compiled = setupCompiledUnit(config)
      val configValue = StringValue(config.name)
      val _ = compiled.optimized
      compiled.allOptimizationStats.flatMap { case (optimName, vs) =>
        vs.flatMap {
          case (metric, value: Int) =>
            Some(IndexedSeq(configValue, StringValue(optimName), StringValue(metric), IntValue(value)))
          case (metric, value: Long) =>
            Some(IndexedSeq(configValue, StringValue(optimName), StringValue(metric), LongValue(value)))
          case (metric, value: Double) =>
            Some(IndexedSeq(configValue, StringValue(optimName), StringValue(metric), DoubleValue(value)))
          case (metric, value: String) =>
            Some(IndexedSeq(configValue, StringValue(optimName), StringValue(metric), StringValue(value)))
          case (metric, value) =>
            println(s"Unsupported metric `$metric` with value `$value`")
            None
        }
      }
    }
    val ds = Dataset("Optimizations", IndexedSeq("Config", "Name", "Kind", "Value"), rows)
    if (storeFiles) FileUtil.writeFile(getOutFile(s"${name}_optimization_stats.csv"), ds.toCSV())
    ds

  private def runPerformanceBenchmark(config: Config, readRel: Relation, runs: Int, warmups: Int): Seq[(Int, Long)] =
    collectGarbage()

    // Warmup
    for (k <- Range.inclusive(1, warmups)) {
      val engine = setupEngine(config)
      try {
        engine.read(readRel)
      } catch { case exec =>
          println(s"Warmup execution with config: $config failed with error: $exec")
      }
    }

    collectGarbage()

    // Run
    for (j <- Range.inclusive(1, runs)) yield {
      val engine = setupEngine(config)
      try {
        val diff = engine.measure(readRel)
        collectGarbage()
        j -> diff
      } catch { case exec =>
          println(s"Measurement with config: $config failed with error: $exec")
          j -> 0 //Long.MinValue
      }
    }

  def measurePerformance(readRel: Relation, runs: Int, warmups: Int): Dataset =
    val rows = configs.flatMap { config =>
      val configValue = StringValue(config.name)
      val measurement = runPerformanceBenchmark(config, readRel, runs, warmups)
      measurement.map(m => IndexedSeq(configValue, IntValue(m._1), LongValue(m._2)))
    }
    val ds = Dataset("Runtime", IndexedSeq("Config", "Run", "Ns"), rows)
    if (storeFiles) FileUtil.writeFile(getOutFile(s"${name}_performance.csv"), ds.toCSV())
    ds

  private def convertNanoSeconds(d: Double, to: TimeUnit) = to match
    case util.TimeUnit.Second => d * (1.0 / 1_000_000_000.0)
    case util.TimeUnit.Milli => d * (1.0 / 1_000_000.0)
    case util.TimeUnit.Micro => d * (1.0 / 1000.0)
    case util.TimeUnit.Nano => d

  def boxPlotPerformance(ds: Dataset, timeUnit: TimeUnit, openPlot: Boolean = false): Unit =
    val barOptions = BarplotOptionsBuilder()
      .setColor(Blue)
      .setTitle(name)
      .setXAxisLabel("Config")
      .setYAxisLabel(s"Runtime ($timeUnit)")
      .build()
    val groupedByConfig = ds.data.groupBy(_.head)
    val plotData = groupedByConfig.map {
      case (StringValue(configTitle), rows) =>
        val timesInNs = rows.map(_.last.asInstanceOf[LongValue].v.toDouble)
        val avgNs = timesInNs.sum / timesInNs.size
        BarConfig(configTitle) -> convertNanoSeconds(avgNs, timeUnit)
    }.toSeq.sortBy(_._1.label)
    val plot = Plotter.barplot(barOptions, plotData)

    // save csv and image
    val imgFile = getOutFile(s"$name.pdf")
    plot.save(imgFile)

    if (openPlot)
      Desktop.getDesktop.open(imgFile)
