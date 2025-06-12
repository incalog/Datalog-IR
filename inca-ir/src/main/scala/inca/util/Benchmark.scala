package inca.util

import benchmark.plot.{BarConfig, Plotter}
import benchmark.plot.option.Color.Blue
import benchmark.plot.option.Position.{TopLeft, TopRight}
import benchmark.plot.option.{Color, GroupedBarplotOptions}
import benchmark.plot.option.builder.{BarplotOptionsBuilder, GroupedBarplotOptionsBuilder}
import benchmark.util
import benchmark.util.{Dataset, DoubleValue, IntValue, LongValue, StringValue, TimeUnit}
import inca.ir.CompiledUnit
import inca.ir.execution.{ExecutorEngine, Relation}
import inca.ir.visitors.StatisticsCollector

import java.awt.Desktop
import java.io.{File, IOException}
import java.util.regex.Pattern

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

// TODO: Introduce line plots (by also using category and group?)
//  Maybe we can introduce a different Abstraction / trait for that?
trait Benchmark[Config <: BenchmarkConfig, CUnit <: CompiledUnit]:
  val name: String
  val configs: Seq[Config]
  val outDir: Option[File]

  val storeIntermediateFiles: Boolean = outDir.isDefined

  def getOutFile(fileName: String): File =
    outDir match
      case Some(dir) =>
        dir.mkdirs()
        File(s"${dir.getAbsolutePath}/$fileName")
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
    if (storeIntermediateFiles) FileUtil.writeFile(getOutFile(s"${name}_relations_stats.csv"), ds.toCSV())
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
    if (storeIntermediateFiles) FileUtil.writeFile(getOutFile(s"${name}_stats.csv"), ds.toCSV())
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
    if (storeIntermediateFiles) FileUtil.writeFile(getOutFile(s"${name}_optimization_stats.csv"), ds.toCSV())
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

  def measurePerformance(readRel: Relation, runs: Int, warmups: Int, topK: Int): Dataset =
    val rows = configs.flatMap { config =>
      val configValue = StringValue(config.name)
      val measurement = runPerformanceBenchmark(config, readRel, runs, warmups)
      val topKMeasurements = 0.until(topK).zip(measurement.map(_._2).sorted.take(topK))
      topKMeasurements.map(m => IndexedSeq(configValue, IntValue(m._1), LongValue(m._2)))
    }
    val ds = Dataset("Runtime", IndexedSeq("Config", "Run", "Ns"), rows)
    if (storeIntermediateFiles) FileUtil.writeFile(getOutFile(s"${name}_performance.csv"), ds.toCSV())
    ds

  private def convertNanoSeconds(d: Double, to: TimeUnit) = to match
    case util.TimeUnit.Second => d * (1.0 / 1_000_000_000.0)
    case util.TimeUnit.Milli => d * (1.0 / 1_000_000.0)
    case util.TimeUnit.Micro => d * (1.0 / 1000.0)
    case util.TimeUnit.Nano => d

  /**
   * Measure the performance and create a grouped bar plot for it. We assume the csv file has the following format:
   *
   * | Config | Run | Time in NS
   * ----------------------------
   * | category_1$delimiter$group_1 | ...
   * | category_1$delimiter$group_1 | ...
   * | category_1$delimiter$group_2 | ...
   * ...
   * | category_n$delimiter$group_1 | ...
   * | category_n$delimiter$group_1 | ...
   * ...
   * | category_n$delimiter$group_2 | ...
   *
   * This will generate the following plot:
   *
   * Runtime
   *   |
   *   |
   *   |____________________________________________ Engine
   *       group_1  group_2  ...  group_1  group_2
   *         category_1      ...     category_n
   *
   * Note the config name must consist of two components. The first element is the category and the second element is
   * the group. Both are separated by the delimiter.
   *
   * E.g. Viatra optimized ~> delimiter " " ~>
   * Category: Viatra
   * Group: optimized
   *
   * Further, we assume that all groups are shared across the categories.
   */
  def groupedBarPlotPerformance(ds: Dataset, timeUnit: TimeUnit, delimiter: String, openPlot: Boolean = false, xLabel: Option[String] = None): File =
    val colors = Color.values
    val groupedByConfig = ds.data.groupBy(_.head).toSeq
    val plotData = groupedByConfig.foldLeft(Map[String, Seq[(String, Double)]]()) {
      case (acc, (StringValue(configTitle), rows)) =>
        val timesInNs = rows.map(_.last.asInstanceOf[LongValue].v.toDouble)
        val avgNs = timesInNs.sum / timesInNs.size
        val components = configTitle.split(Pattern.quote(delimiter))
        if (components.size != 2)
          throw IllegalStateException(s"Expected 2 components but got: ${components.mkString(", ")}")
        val category = components.head
        val group = components.last
        val existing = acc.getOrElse(category, Seq())
        acc + (category -> (existing :+ (group -> convertNanoSeconds(avgNs, timeUnit))))
      case _ => throw IllegalArgumentException("Malformed plot data!")
    }.toSeq.sortBy(_._1)
    val groups = plotData.head._2.map(_._1).sorted
    if (plotData.exists(_._2.map(_._1).toSet != groups.toSet))
      throw IllegalArgumentException("Groups do not match!")
    val groupedPlotData = plotData.map
    val groupedBarplotOptions = GroupedBarplotOptionsBuilder()
      .setGroups(groups.zip(colors))
      .setLegendPosition(TopRight)
      .setTitle(name)
      .setXAxisLabel(xLabel.getOrElse("Config"))
      .setYAxisLabel(s"Runtime ($timeUnit)")
      .build()
    val plot = Plotter.groupedBarplot(groupedBarplotOptions, plotData.map((k, v) => BarConfig(k) -> v.map(_._2)))

    val imgFile = getOutFile(s"$name.pdf")
    plot.save(imgFile)

    if (storeIntermediateFiles) FileUtil.writeFile(getOutFile(s"$name.R"), plot.genScript(imgFile))

    if (openPlot) Desktop.getDesktop.open(imgFile)

    imgFile

  /**
   * Create a bar plot with one bar for each configuration in the file. We assume the csv file has the following format:
   *
   * | Config | Run | Time in NS
   * ----------------------------
   * | config_1 | ...
   * | config_1 | ...
   * ...
   * | config_n | ...
   *
   * This will generate the following plot:
   *
   *  Runtime
   *   |
   *   |
   *   |________________________________ config
   *     config_1      ...     config_n
   */
  def barPlotPerformance(ds: Dataset, timeUnit: TimeUnit, openPlot: Boolean = false, xLabel: Option[String] = None): File =
    val barOptions = BarplotOptionsBuilder()
      .setColor(Blue)
      .setTitle(name)
      .setXAxisLabel(xLabel.getOrElse("Config"))
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

    val imgFile = getOutFile(s"$name.pdf")
    plot.save(imgFile)

    if (storeIntermediateFiles) FileUtil.writeFile(getOutFile(s"$name.R"), plot.genScript(imgFile))

    if (openPlot) Desktop.getDesktop.open(imgFile)

    imgFile
