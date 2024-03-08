package inca.frontend.objectoriented.measurements

// Make sure to increase memory and stack size by setting the JVM Options for the run config:
// -Xms12288m -Xss1000m

object Benchmark {
  def main(args: Array[String]): Unit = {
    // Configure those values
    val warmups = 0
    val runs = 1

    // Micro-benchmark Interpreter - Section 3
    /*val pathBenchmark = PathBenchmark(warmups, runs)
    pathBenchmark.run("left")
    pathBenchmark.run("right")

    // Micro-benchmark - Section 3 (different representations of the mutation counter)
    val mutationBenchmark = MutationBenchmark(warmups, runs)
    mutationBenchmark.run()*/

    // Dependency Analysis - Section 8
    val asgBenchmark = ASGBenchmark(warmups, runs)
    asgBenchmark.run()

    // Sign + Constant Analysis - Section 8
    /*val fsBenchmark = FSAnalysisBenchmark(warmups, runs)
    fsBenchmark.runSign()
    fsBenchmark.runConstant()*/

    /** Outdated */
    //val cfgBenchmark = CFGBenchmark(warmups, runs)
    //cfgBenchmark.run()

    //val whileBenchmark = WhileBenchmark(warmups, runs)
    //whileBenchmark.run()
  }
}
