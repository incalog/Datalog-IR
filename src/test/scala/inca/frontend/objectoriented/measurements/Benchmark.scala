package inca.frontend.objectoriented.measurements

object Benchmark {
  def main(args: Array[String]): Unit = {
    val warmups = 2
    val runs = 3

    val fsBenchmark = FSConstantAnalysisBenchmark(warmups, runs)
    fsBenchmark.run()

    //val mutationBenchmark = MutationBenchmark(warmups, runs)
    //mutationBenchmark.run()

    //val cfgBenchmark = CFGBenchmark(warmups, runs)
    //cfgBenchmark.run()

    //val whileBenchmark = WhileBenchmark(warmups, runs)
    //whileBenchmark.run()

    //val asgBenchmark = ASGBenchmark(warmups, runs)
    //asgBenchmark.run()

    //val pathBenchmark = PathBenchmark(warmups, runs)
    //pathBenchmark.run("left")
    //pathBenchmark.run("right")
  }
}
