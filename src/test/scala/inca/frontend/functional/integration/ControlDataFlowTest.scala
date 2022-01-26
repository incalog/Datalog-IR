package inca.frontend.functional.integration

import inca.backend.transform.magic.demand.DemandTransformation.demandPatternPrefix
import inca.compiler.Compiler
import inca.examples.functional.ControlDataFlow
import inca.frontend.functional.compiler.{CompiledFunctionalModule, FunctionalOptions}
import inca.frontend.functional.executor.FunctionalExecutor._
import inca.runtime.EnginePool
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuple
import org.scalatest.funsuite.AnyFunSuite
import truechange.EditScript

class ControlDataFlowTest extends AnyFunSuite {

  test("flow ex 2.1") {
    val fun = loadFunction(ControlDataFlow.cflowModule)
    val input = fun.input(Seq(ControlDataFlow.example_2_1))
    assert(fun.executeInput("flow", input).res.size == 4)
    assert(fun.output("flowR", input._2).res.isEmpty)
//    fun.printAllMatches()
  }

  test("flowR ex 2.1") {
    val fun = loadFunction(ControlDataFlow.cflowModule)
    val input = fun.input(Seq(ControlDataFlow.example_2_1))
    assert(fun.executeInput("flowR", input).res.size == 4)
    assert(fun.output("flow", input._2).res.size == 4)
//    fun.printAllMatches()
  }

  test("transitiveFlow ex 2.1") {
    val fun = loadFunction(ControlDataFlow.cflowModule)
    val input = fun.input(Seq(ControlDataFlow.example_2_1))
    assert(fun.executeInput("transitiveFlow", input).res.size == 12)
//    fun.printAllMatches()
  }

//  test("available expressions ex 2.4") {
//    val fun = loadFunction(ControlDataFlow.AEModule)
//    val input = fun.input(ControlDataFlow.example_2_4)
//    assert(fun.executeInput("final_AE", input).res.size == 1)
//    assert(fun.executeInput("allEntries_AE", input).res.size == 3)
//    assert(fun.executeInput("allExits_AE", input).res.size == 5)
//    fun.printAllMatches()
//    fun.output("allExits_AE", input._2).res.foreach { case Seq(c1, c2) => println(s"$c2 in $c1") }
//  }

//  test("reaching definitions ex 2.7") {
//    val fun = loadFunction(ControlDataFlow.RDmodule)
//    val input = fun.input(ControlDataFlow.example_2_7)
//
//    val rels = fun.compiled.optimized.pats
//    println("relations: " + rels.size)
//    println("input relations: " + rels.count(_.name.contains(demandPatternPrefix)))
//    println("bodies: " + rels.flatMap(_.bodies).size)
//    println("atoms: " + rels.flatMap(_.bodies.flatMap(_.atoms)).size)
//
//    assert(fun.executeInput("final_RD", input).res.size == 4)
//    assert(fun.executeInput("allEntries_RD", input).res.size == 15)
//    assert(fun.executeInput("allExits_RD", input).res.size == 13)
//
////    fun.printAllMatches()
//    fun.output("allExits_RD", input._2).res.foreach { case Seq(c1, c2, c3) => println(s"$c2:$c3 in $c1") }
//  }

//  test("intervals ex 2.7") {
//    println(ControlDataFlow.IntervalModule.lines().count())
//    val fun = loadFunction(ControlDataFlow.IntervalModule)
//    println(fun.compiled.ir.copy(scalaContent = Seq()).toString.lines().count())
//    println(fun.compiled.optimized.copy(scalaContent = Seq()).toString.lines().count())
//    println(fun.compiled.psystemSource.toString.lines().count())
//  }

  test("intervals ex 2.7") {
    val fun = loadFunction(ControlDataFlow.IntervalModule)

    // fun.compiled.printStatistics()
    val prog = fun.input(ControlDataFlow.example_2_7)

    val rels = fun.compiled.optimized.pats
    println("relations: " + rels.size)
    println("input relations: " + rels.count(_.name.contains(demandPatternPrefix)))
    println("bodies: " + rels.flatMap(_.bodies).size)
    println("atoms: " + rels.flatMap(_.bodies.flatMap(_.atoms)).size)

    val res = fun.executeInput("final_var", prog)
//    fun.printAllMatches()
    // TODO second aggregation currently implemented by hand in exit_var_external, should be generated eventually
    assert(res.res.size == 2)
  }

  test("aeval") {
    val fun = loadFunction(ControlDataFlow.AEvalModule)
    fun.compiled.printStatistics()
//    println(fun.compiled.optimized)

//    val names = Set("aeval", "add","sub","greaterThan","mul")
//    val m = fun.compiled.optimized.copy(pats = fun.compiled.optimized.pats.filter(p => names.exists(n => p.name.contains(n))), scalaContent = Seq())
//    println(m)
//    println(s"GP relations: ${m.pats.size}")
//    println(s"GP bodies: ${m.pats.map(_.bodies.size).sum}")

//    val prog = fun.input(ControlDataFlow.example_2_7)
//
//    val rels = fun.compiled.optimized.pats
//    println("relations: " + rels.size)
//    println("input relations: " + rels.filter(_.name.contains(DemandTransformation.demandPatternPrefix)).size)
//    println("bodies: " + rels.flatMap(_.bodies).size)
//    println("constraints: " + rels.flatMap(_.bodies.flatMap(_.constraints)).size)
//
//    val res = fun.executeTuple("final_var", Tuples.flatTupleOf(prog))
//    fun.printAllMatches()
//    // TODO second aggregation currently implemented by hand in exit_var_external, should be generated eventually
//    assert(res.res.size == 2)
  }

  test("powerset dataflow analysis 1") {
    val fun = loadFunction(ControlDataFlow.IntValuesModule)
    val res = fun.executeInput("final_var", fun.input(ControlDataFlow.exampleDataflow1))
    assert(res.res.size == 100)
  }

  test("powerset dataflow analysis 2") {
    val fun = loadFunction(ControlDataFlow.IntValuesModule)
    val res = fun.executeInput("final_var", fun.input(ControlDataFlow.exampleDataflow2))
    assert(res.res.size == 200)
  }
}

object RunInitial extends App {
  val compiled = compileFunction(ControlDataFlow.IntValuesModule)
  val runs = 100
  var edits: EditScript = null
  var tuple: Tuple = null
  for (i <- 0 until runs) {
    val fun = loadFunction(compiled)
    if (edits == null) {
      val input = fun.input(ControlDataFlow.exampleDataflow1)
      edits = input._1
      tuple = input._2
    }
    val (load, insert, delete) = fun.measure("final_var", edits, tuple)
    println(s"${load / 1000 / 1000}, ${insert / 1000 / 1000}, ${delete / 1000 / 1000}")
    EnginePool.disposeAllEngines()
  }
}

object RunIncremental extends App {
  import inca.frontend.functional.executor.IncrementalFunctionalExecutor

  val compiled = IncrementalFunctionalExecutor.compileFunction(ControlDataFlow.IntValuesModule)
  println(compiled.optimized)
  val runs = 100
  val fun = IncrementalFunctionalExecutor.loadFunction(compiled)
  val originProg = ControlDataFlow.exampleDataflow1
  val (edits, tuple) = fun.input(originProg)
  val (load, insert, delete, _) = fun.measureInitial("final_var", edits, tuple)
  println(s"IN ${load / 1000 / 1000}, ${insert / 1000 / 1000}, ${delete / 1000 / 1000}")
  // incremental measurements
  for (i <- 0 until runs) {
    // measure change
    val changedProg = ControlDataFlow.exampleDataflow1Change1
    val (edits1, tuple1) = fun.input(changedProg)
    val (load1, insert1, delete1, _) = fun.measureUpdate("final_var", edits1, tuple1)

    // measure revert of change
    val (edits2, tuple2) = fun.input(originProg)
    val (load2, insert2, delete2, _) = fun.measureUpdate("final_var", edits2, tuple2)
    println(s"${insert1 / 1000 / 1000}, ${insert2 / 1000 / 1000}")
  }
  EnginePool.disposeAllEngines()
}
