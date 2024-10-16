package inca.frontend.functional.executor.itypes

import inca.frontend.functional.compile.{CompiledFunctionalUnit, FunctionalCompilerOptions}
import inca.frontend.functional.executor.FunctionalExecutor
import inca.ir.execution.{Relation, UnitRelation}
import inca.util.FileUtil
import inca.ascent.backend.Executor
import inca.ir.execution.ThreadCount.Fixed
import org.scalatest.funsuite.AnyFunSuite
import inca.ir.extension.{aggregateset, block, bool, datamatch, demand, disjunction, map, not, set, tuple, typeparam}
import inca.ir.valueNumbering.ValueNumbering
import inca.util.CSVUtil.csvToString


class FunctionalAscentExecutorTest extends AnyFunSuite:
  val options = FunctionalCompilerOptions.fromResource("functional/Options.ini")
  val exec: FunctionalExecutor = new FunctionalExecutor(new Executor(Fixed(1)))

  test("TypeChecker") {
    val code = FileUtil.readFileFromResource("functional/itypes/TypeChecker.finca")
    val compiled = exec.compileFunction(code, options)
    compiled.setPipeline(CompiledFunctionalUnit.pipeline)
    val loaded = exec.loadFunction(compiled)
    //val diff = loaded.engine.measure(UnitRelation("main"))
    //println(diff)
    //println(loaded.engine.readAll().map(_.size).sum)
    val res = loaded.execute("main", Seq())
    assertResult(
      "Some_Type(TFun(TFun(TFun(TInt, TInt), TFun(TInt, TInt)), TFun(TFun(TFun(TInt, TInt), TFun(TInt, TInt)), TFun(TFun(TInt, TInt), TFun(TInt, TInt)))))"
    )(
      res.entries.head.toString
    )
  }



@main def benchmarkTypesAscent() ={
  val warmups = 3
  val runs = 10
  val resultPath = "benchmark/itypes/withVN/ascent"

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

      () => new ValueNumbering {}
    ))
    //    println(compiled)
    val loaded = exec.loadFunction(compiled)

    val measured = loaded.engine.measure(UnitRelation("main"))
    println(s"run $i done: measured $measured")
    measured
  }

  val results: Seq[(String, IndexedSeq[Long])] = (warmups until warmups + runs).map(
    i => i.toString -> IndexedSeq(measurements(i))
  ).appended("average" -> IndexedSeq(measurements.drop(warmups).sum / runs))

  FileUtil.writeFile(s"$resultPath/itypes_withVN_ascent.csv", csvToString(toCSV(results)))
}



