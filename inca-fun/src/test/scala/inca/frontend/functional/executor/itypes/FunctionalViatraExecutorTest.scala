package inca.frontend.functional.executor.itypes

import inca.frontend.functional.compile.{CompiledFunctionalModule, FunctionalCompilerOptions}
import inca.frontend.functional.executor.FunctionalExecutor
import inca.util.FileUtil
import inca.ir.execution.{Relation, UnitRelation}
import inca.ir.valueNumbering.ValueNumbering
import inca.viatra.backend.Executor
import org.scalatest.funsuite.AnyFunSuite
import inca.ir.extension.{aggregateset, block, bool, datamatch, demand, disjunction, map, not, set, tuple, typeparam}
import inca.util.CSVUtil.csvToString


class FunctionalViatraExecutorTest extends AnyFunSuite:
  val options = FunctionalCompilerOptions.fromResource("functional/Options.ini")
  val exec: FunctionalExecutor = new FunctionalExecutor(new Executor)

  test("TypeChecker") {
    val code = FileUtil.readFileFromResource("functional/itypes/TypeChecker.finca")
    val compiled = exec.compileFunction(code, options)
    compiled.setPipeline(CompiledFunctionalModule.pipeline)
    val loaded = exec.loadFunction(compiled)
    println(loaded.engine.measure(UnitRelation("main")))
    //println(loaded.engine.readAll().map(_.size).sum)
    val res = loaded.execute("main", Seq())
    assertResult(
      "Some$Type(TFun(TFun(TFun(TInt(),TInt()),TFun(TInt(),TInt())),TFun(TFun(TFun(TInt(),TInt()),TFun(TInt(),TInt())),TFun(TFun(TInt(),TInt()),TFun(TInt(),TInt())))))"
    )(
      res.entries.head.toString
    )
  }

@main def benchmarkTypesViatra() ={
  val warmups = 3
  val runs = 10
  val resultPath = "benchmark/itypes/withVN/viatra"

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

  FileUtil.writeFile(s"$resultPath/itypes_withVN_viatra.csv", csvToString(toCSV(results)))
}

// copied from inca.casestudy.util
def toCSV(vals: Seq[(String, IndexedSeq[Long])]): IndexedSeq[IndexedSeq[Any]] = {
  val header = vals.map(_._1).toIndexedSeq
  // we assume that each list has same number of elements
  val rowLength = vals.head._2.size
  val rows = for (i <- 0 until rowLength) yield vals.map(_._2(i)).toIndexedSeq
  header +: rows
}