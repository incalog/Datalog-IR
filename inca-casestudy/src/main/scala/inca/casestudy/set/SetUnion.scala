package inca.casestudy.set

import inca.casestudy.util.Util.{collectGarbage, toCSV}
import inca.ir.*
import inca.ir.execution.ThreadCount.Fixed
import inca.ir.execution.{IRExecutor, UnitRelation}
import inca.ir.extension.*
import inca.ir.extension.arithmetic.*
import inca.ir.extension.demand.*
import inca.ir.extension.set
import inca.ir.extension.set.SyntacticOptimizer
import inca.ir.optimize.AliasElimination
import inca.ir.util.SourceLocation
import inca.util.{CSVUtil, FileUtil}
import inca.util.compileroptions.CompilerOptions
import inca.viatra.backend.Executor

// plotting
import breeze.linalg._
import breeze.plot._

import scala.language.implicitConversions

def factorial(n: Int) = (1 to n).product

object SetUnion:
  val n = 6 // choose a big enough number
  val combinations = 2*factorial(n)
  println(s"Number of unique sets: $combinations")

  val a = Range(1, n+1).map(arithmetic.IntNum.apply)
  val b = Range(n+1, (n*2)+1).map(arithmetic.IntNum.apply)

  def main(numSets: Int) = Relation("main",
    Seq(
      Param("s", set.TSet(arithmetic.TInt)),
      Param("v", arithmetic.TInt),
    ),
    Seq(
      Body(Seq(
        Eq(
          Var("s"),
          // We represent sets with case classes, that is make them unique by permutation
          set.SetUnion(a.permutations.zip(b.permutations).flatten {case (a,b) => Seq(a,b) }.toSeq.take(numSets).map(set.SetLit.apply))
        ),
        set.SetMember(Var("v"), Var("s"))
      ))
    )
  )


  def createMod(numSets: Int) = Module("SetUnionMicro", BaseIR.language + arithmetic.IR + set.IR,
    Seq(
      main(numSets),
    )
  )


  def createCompiled(mod: Module, optimizeSets: Boolean): CompiledModule = new CompiledModule:
    override def name: Name = mod.name
    override def sourceLocation: SourceLocation = SourceLocation.NoSourceLocation
    override def ir: Module = mod
    override def compilerOptions: CompilerOptions = {
      val opt = CompilerOptions.default
      opt.irLogging.logModule = false
      opt.irLogging.logLowerings = false
      opt.irLogging.logTypeInformation = false
      opt.irLogging.logStatsAfterOptimizations = false
      opt
    }

    val optim = if optimizeSets then
      List(() => new SyntacticOptimizer {})
    else
      List()

    setPipeline(optim ++ List(
      () => new set.Lowering {},
      () => new block.Lowering {},
      () => new disjunction.Lowering {},
      () => new demand.Lowering {},
      () => new tuple.Lowering {},
      () => new AliasElimination {}
    ))


  private def run(executor: IRExecutor, maxNumberSet: Int = 200, steps: Int = 20, warmups: Int = 0, runs: Int = 1): Seq[(Double, Double)] =
    // combinations + 1
    for (i <- Range.inclusive(2, maxNumberSet, steps)) yield {
      val mod = createMod(i)

      for (j <- Range(0, warmups)) {
        println(s"Warmup: $j")
        val compiled = createCompiled(mod, false)
        val engine = executor.instantiate(compiled)
        engine.measure(UnitRelation("main")).toDouble
        collectGarbage()
      }

      println(s"Number of sets: $i")

      val diffs = for (k <- Range(0, runs)) yield {
        println(s"\tRun: $k")
        val compiled = createCompiled(mod, false)
        val engine = executor.instantiate(compiled)
        //val rel = engine.read(UnitRelation("main"))
        //println(rel.asTable)
        engine.measure(UnitRelation("main")).toDouble
      }
      (((i + 5) / 10) * 10).toDouble -> diffs.sum / diffs.size
    }

  def plotResult(res: Map[String, Seq[(Double, Double)]], file: String): Unit =
    val f = Figure()
    val p = f.subplot(0)

    res.foreach { (name, r) =>
      val (x, y) = r.unzip
      val timeInMS = y.map(ns => ns / 1000000)
      p += plot(DenseVector(x: _*), DenseVector(timeInMS: _*), name=name)
    }

    p.xlabel = "Number of Sets"
    p.ylabel = "Running time (ms)"
    p.legend = true

    f.saveas(file)

  @main def plotSetUnionFromCSV() = {
    val csvFile = "benchmark/SetUnion/result.csv"
    val content = FileUtil.readFile(csvFile)
    val csv = CSVUtil.fromCSV(content, skipHeader = true)
    val (souffleRes, viatraRes, ascentRes) = csv.map {
      case IndexedSeq(i: String, souffle: String, viatra: String, ascent: String) =>
        ((i.toDouble, souffle.toDouble), (i.toDouble, viatra.toDouble), (i.toDouble, ascent.toDouble))
    }.unzip3

    plotResult(Map(
      "Souffle" -> souffleRes,
      //"Viatra" -> viatraRes,
      "Ascent" -> ascentRes
    ), "benchmark/SetUnion/graph.pdf")
  }

  @main def runAndPlotSetUnion() = {
    val souffleRes = run(inca.souffle.backend.Executor(Fixed(1)))
    val viatraRes = run(inca.viatra.backend.Executor(), warmups = 3, runs = 5)
    val ascentRes = run(inca.ascent.backend.Executor(Fixed(1)))

    val headerLine = IndexedSeq("NumberOfSets", "Souffle", "Viatra", "Ascent")
    val rows = for (i <- Range(0, souffleRes.size)) yield {
        IndexedSeq(souffleRes(i)._1, souffleRes(i)._2, viatraRes(i)._2, ascentRes(i)._2)
    }
    FileUtil.writeFile("benchmark/SetUnion/result.csv", CSVUtil.csvToString(headerLine +: rows))

    plotResult(Map(
      "Souffle" -> souffleRes,
      "Viatra" -> viatraRes,
      "Ascent" -> ascentRes
    ), "benchmark/SetUnion/graph.pdf")
  }

  