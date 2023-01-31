package inca.measurements.util

import inca.measurements.util.CSVUtil.csvRowToString
import inca.measurements.util.CSVUtil.CSVRow
import java.io.File
import java.io.PrintWriter
import scala.io.Source

object BenchmarkUtils {

  object Measurement {
    def apply(
        name: String,
        vals: Seq[Long],
        extra: Map[String, Any] = Map()
      )(implicit timing: Timing
      ): Measurement = {
      val outliers = timing.outliers
      val measurementVals = vals.drop(timing.discard)
      Measurement(name, measurementVals, outliers, extra)
    }
  }

  case class Measurement(name: String, vals: Seq[Long], outliers: Int, extra: Map[String, Any]) {
    val valsWithoutOutliers: Seq[Long] = {
      val ordered = vals.sorted
      ordered.dropRight(outliers)
    }

    override def toString: String = {
      val diffTime = avg(valsWithoutOutliers)
      // edit size
      // diff time
      val text = s"""
        |Measurement $name
        |  Diffing time (ms): ${ms(diffTime)}""".stripMargin
      if (extra.isEmpty)
        text
      else
        text + extra.map(kv => s"\n  ${kv._1}: ${kv._2}").foldLeft("")(_ + _)
    }

    def combine(name: String, other: Measurement): Measurement = {
      Measurement(name, vals ++ other.vals, outliers = 0, Map())
    }

    def extend(newExtras: Map[String, Any]): Measurement = {
      Measurement(name, vals, outliers, extra ++ newExtras)
    }

    val csvHeader: String =
      s"Name, AVG time (ms)${if (extra.isEmpty) "," else extra.keys.mkString(",", ",", ",")}raw data (ns)"

    val csv: CSVRow = {

      val diffTime = ms(avg(valsWithoutOutliers))
      //      s"$name, $srcSize, $destSize, ${editScript.size}, $diffTime${if (extra.isEmpty) ", " else extra.values.mkString(", ", ", ", ", ")}${BenchmarkUtils.toCSVRow(vals)}"
      IndexedSeq(name, diffTime) ++ extra.values ++ valsWithoutOutliers
    }
  }

  def measurementsToCSV(measurements: Seq[Measurement]): String =
    if (measurements.isEmpty) ""
    else
      measurements.head.csvHeader + "\n" + measurements.map { m => csvRowToString(m.csv) }.mkString(
        "\n"
      )

  def ms(l: Double): Double = l / 1000 / 1000

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

  def avg[T](vals: Seq[T])(implicit num: Numeric[T]): Double =
    if (vals.isEmpty) 0 else num.toDouble(vals.sum) / vals.size
}
