package inca.frontend.objectoriented.measurements

object Benchmark {
  def main(args: Array[String]): Unit = {
    val warmups = 0
    val runs = 1

    //val asgBenchmark = ASGBenchmark(warmups, runs)
    //asgBenchmark.run()

    val pathBenchmark = PathBenchmark(warmups, runs)
    //pathBenchmark.run("left")
    pathBenchmark.run("right")
  }
}
