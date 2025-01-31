package inca.frontend.functional.executor.itypes

import inca.souffle.backend.Executor
import inca.frontend.functional.compile.{CompiledFunctionalUnit, FunctionalCompilerOptions}
import inca.frontend.functional.executor.FunctionalExecutor
import inca.ir.execution.ThreadCount.Fixed
import inca.ir.execution.{Relation, UnitRelation}
import inca.ir.extension.{aggregateset, block, bool, datamatch, demand, disjunction, map, not, set, tuple, typeparam}
import inca.util.CSVUtil.csvToString
import inca.util.FileUtil
import org.scalatest.funsuite.AnyFunSuite
import inca.ir.valueNumbering.ValueNumbering


class FunctionalSouffleExecutorTest extends AnyFunSuite:
  val options = FunctionalCompilerOptions.fromResource("functional/Options.ini")
  val exec: FunctionalExecutor = new FunctionalExecutor(new Executor(Fixed(1)))

  test("TypeChecker") {
    val code = FileUtil.readFileFromResource("functional/itypes/TypeChecker.finca")
    val compiled = exec.compileFunction(code, options)
    compiled.setPipeline(CompiledFunctionalUnit.pipeline)
    val loaded = exec.loadFunction(compiled)
    //println(loaded.engine.measure(UnitRelation("main")))
    //println(loaded.engine.readAll().map(_.size).sum)
    val res = loaded.execute("main", Seq())
    assertResult(
      "$Some_Type($TFun($TFun($TFun($TInt, $TInt), $TFun($TInt, $TInt)), $TFun($TFun($TFun($TInt, $TInt), $TFun($TInt, $TInt)), $TFun($TFun($TInt, $TInt), $TFun($TInt, $TInt)))))"
    )(
      res.entries.head.toString
    )
  }



@main def benchmarkTypesSouffle() ={
  val warmups = 3
  val runs = 10
  val resultPath = "benchmark/itypes/withVN/souffle"

  val measurements = for (i <- Range.inclusive(1, warmups + runs)) yield {
    val options = FunctionalCompilerOptions.fromResource("functional/Options.ini")
    val exec: FunctionalExecutor = new FunctionalExecutor(new Executor)
    val code = FileUtil.readFileFromResource("functional/itypes/TypeChecker.finca")
    val compiled = exec.compileFunction(code, options)
    compiled.setPipeline(List(
      () => new typeparam.Lowering {},
      () => new aggregateset.Lowering {},
      () => new set.Lowering {},
      () => new map.Lowering {},
      //    () => new bool.Optimizer {},
      () => new bool.Lowering {},
      () => new datamatch.Lowering {},
      () => new block.Lowering {},
      () => new disjunction.Lowering {},
      () => new not.Lowering {},
      () => new demand.Lowering {},
      () => new tuple.Lowering {},

      () => new ValueNumbering{}
    ))
    //    println(compiled)
    val loaded = exec.loadFunction(compiled)

    val measured = loaded.engine.measure(UnitRelation("main"))
    println(s"run $i done: measured $measured")
    measured
  }

  val results: Seq[(String, IndexedSeq[Long])] = (warmups until warmups+runs).map(
    i => i.toString -> IndexedSeq(measurements(i))
  ).appended("average" -> IndexedSeq(measurements.drop(warmups).sum / runs))

  FileUtil.writeFile(s"$resultPath/itypes_withVN_souffle.csv", csvToString(toCSV(results)))
}
