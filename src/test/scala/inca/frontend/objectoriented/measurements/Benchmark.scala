package inca.frontend.objectoriented.measurements

object Benchmark {
  def main(args: Array[String]): Unit = {
    val warmups = 0
    val runs = 1

    val fsBenchmark = FSAnalysisBenchmark(warmups, runs)
    fsBenchmark.runSign()

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
