package inca.measurements.util

import inca.measurements.util.BenchmarkUtils.Timing

trait Config {
  val warmup: Int
  val runs: Int
  def name: String
  def timing: Timing = Timing(warmup, runs)
}
