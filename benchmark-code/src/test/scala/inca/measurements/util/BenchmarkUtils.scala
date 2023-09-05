package inca.measurements.util

import inca.measurements.util.CSVUtil.csvRowToString
import inca.measurements.util.CSVUtil.CSVRow
import inca.measurements.util.Units.MeasurementUnit

object BenchmarkUtils {

  object Measurement {
    def apply(
        name: String,
        unit: MeasurementUnit,
        vals: Seq[Long],
        extra: Map[String, Any] = Map()
      )(implicit timing: Timing
      ): Measurement = {
      val outliers = timing.outliers
      val measurementVals = vals.drop(timing.discard)
      Measurement(name, unit, measurementVals, outliers, extra)
    }
  }

  case class Measurement(
      name: String,
      unit: MeasurementUnit,
      vals: Seq[Long],
      outliers: Int,
      extra: Map[String, Any]) {
    val valsWithoutOutliers: Seq[Long] = {
      val ordered = vals.sorted
      ordered.dropRight(outliers)
    }

    override def toString: String = {
      val avgTime = avg(valsWithoutOutliers)
      val text = s"""
        |Measurement $name
        |  ${unit.toString}: ${avgTime}""".stripMargin
      if (extra.isEmpty)
        text
      else
        text + extra.map(kv => s"\n  ${kv._1}: ${kv._2}").foldLeft("")(_ + _)
    }

    def combine(name: String, other: Measurement): Measurement = {
      if (unit.isConvertible(other.unit)) {
        Measurement(
          name,
          unit,
          vals ++ other.vals.map(other.unit.convert(_, unit).toLong),
          outliers = 0,
          Map())
      } else {
        throw new IllegalArgumentException("Cannot combine measurements of uncompatible units")
      }
    }

    def extend(newExtras: Map[String, Any]): Measurement = {
      Measurement(name, unit, vals, outliers, extra ++ newExtras)
    }

    val csvHeader: String = {
      s"Name, AVG ${unit.toString} ${if (extra.isEmpty) ","
        else extra.keys.mkString(",", ",", ",")}raw data ${unit.toString}"
    }

    val csv: CSVRow = {
      val avgTime = avg(valsWithoutOutliers)
      IndexedSeq(name, avgTime) ++ extra.values ++ valsWithoutOutliers
    }
  }

  def measurementsToCSV(measurements: Seq[Measurement]): String =
    if (measurements.isEmpty) ""
    else
      measurements.head.csvHeader + "\n" + measurements.map { m => csvRowToString(m.csv) }.mkString(
        "\n"
      )

  def time[R](block: => R): (R, Long) = {
    val t0 = System.nanoTime()
    val result = block // call-by-name
    val t1 = System.nanoTime()
    (result, t1 - t0)
  }

  case class Timing(discard: Int, repeat: Int, outliers: Int = 0)
  def warmup(discard: Int): Timing = Timing(discard, 0)
  def nowarmup(repeat: Int): Timing = Timing(0, repeat)

  def timedNoSetup[R](block: => R)(implicit timing: Timing): (R, Seq[Long]) = {
    val (_, out, _, t) = timed[Unit, R](() => (), _ => block)
    (out, t)
  }

  def timed[A, R](
      setup: () => A,
      block: A => R
    )(implicit timing: Timing
    ): (A, R, Seq[Long], Seq[Long]) = {
    var input = null.asInstanceOf[A]
    var result = null.asInstanceOf[R]

    // discard first runs
    for (_ <- 1 to timing.discard) {
      input = setup()
      val (r, _) = time(block(input))
      result = r
    }

    var setuptimes: Seq[Long] = Nil
    var times: Seq[Long] = Nil
    for (_ <- 1 to timing.repeat) {
      val setupRes = time(setup())
      input = setupRes._1
      val (res, blocktime) = time(block(input))
      result = res
      setuptimes = setuptimes :+ setupRes._2
      times = times :+ blocktime
    }
    (input, result, setuptimes, times)
  }

  def measure[T](run: () => T, config: Config): Seq[T] =
    (for (i <- 0 until config.warmup + config.runs) yield {
      val v = run()
      println(s"Run $i")
      v
    }).drop(config.warmup)

  def avg[T](vals: Seq[T])(implicit num: Numeric[T]): Double =
    if (vals.isEmpty) 0 else num.toDouble(vals.sum) / vals.size
}
