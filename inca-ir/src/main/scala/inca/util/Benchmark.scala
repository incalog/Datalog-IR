package inca.util

import benchmark.plot.{BarConfig, Plotter}
import benchmark.plot.option.Color.Blue
import benchmark.plot.option.Color
import benchmark.plot.option.builder.BarplotOptionsBuilder
import benchmark.util
import benchmark.util.{Dataset, IntValue, LongValue, StringValue, TimeUnit}
import inca.ir.execution.{ExecutorEngine, Relation}

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
trait Benchmark[Config <: BenchmarkConfig]:
  val name: String
  val configs: Seq[Config]
  val outDir: Option[File]

  def storeFiles: Boolean = outDir.isDefined

  def getOutFile(fileName: String): File =
    outDir match
      case Some(dir) => File(s"${dir.getAbsolutePath}/$fileName")
      case _ => File.createTempFile("", fileName)

  def newInstantiatedExecutorEngine(config: Config): ExecutorEngine

  def measureTuples(): Dataset =
    val rows = configs.flatMap { config =>
      val engine = newInstantiatedExecutorEngine(config)
      val outputRels = engine.readAll()
      val configValue = StringValue(config.name)
      val totalTuples = IndexedSeq(configValue, StringValue("Total"), IntValue(outputRels.map(_.size).sum))
      val tuplesPerRelation = outputRels.map { r => IndexedSeq(configValue, StringValue(r.name), IntValue(r.size)) }
      totalTuples +: tuplesPerRelation
    }
    val ds = Dataset("Number of Tuples", IndexedSeq("Config", "Name", "Amount"), rows)
    if (storeFiles) FileUtil.writeFile(getOutFile(s"${name}_tuples.csv"), ds.toCSV())
    ds

  def measureStatistics(): Dataset =
    ???

  private def runPerformanceBenchmark(config: Config, readRel: Relation, runs: Int, warmups: Int): Seq[(Int, Long)] =
    collectGarbage()

    // Warmup
    for (k <- Range.inclusive(1, warmups)) {
      val engine = newInstantiatedExecutorEngine(config)
      try {
        engine.read(readRel)
      } catch { case exec =>
          println(s"Warmup execution with config: $config failed with error: $exec")
      }
    }

    collectGarbage()

    // Run
    for (j <- Range.inclusive(1, runs)) yield {
      val engine = newInstantiatedExecutorEngine(config)
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
