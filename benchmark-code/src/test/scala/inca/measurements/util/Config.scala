package inca.measurements.util

trait Config {
  val warmup: Int
  val runs: Int
  def name: String
}
