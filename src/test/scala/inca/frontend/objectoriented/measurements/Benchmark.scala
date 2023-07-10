package inca.frontend.objectoriented.measurements

object Benchmark {
  def main(args: Array[String]): Unit = {
    val pathBenchmark = PathBenchmark(warmups = 0, runs = 1)
    pathBenchmark.run("left")
    pathBenchmark.run("right")

    //val asgBenchmark = ASGBenchmark(warmups = 0, runs = 1)
    //asgBenchmark.run()
  }
}
