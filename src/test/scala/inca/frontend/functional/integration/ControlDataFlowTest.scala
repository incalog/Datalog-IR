package inca.frontend.functional.integration

import inca.examples.functional.ControlDataFlow
import inca.frontend.functional.executor.FunctionalExecutor._
import org.scalatest.funsuite.AnyFunSuite

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

  test("available expressions ex 2.4") {
    val fun = loadFunction(ControlDataFlow.AEModule)
    val input = fun.input(ControlDataFlow.example_2_4)
    assert(fun.executeInput("final_AE", input).res.size == 1)
    assert(fun.executeInput("allEntries_AE", input).res.size == 3)
    assert(fun.executeInput("allExits_AE", input).res.size == 5)
    fun.printAllMatches()
    fun.output("allExits_AE", input._2).res.foreach { case Seq(c1, c2) => println(s"$c2 in $c1") }
  }

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
//    fun.output("allExits_RD", Tuples.flatTupleOf(prog)).res.foreach { case Seq(c1, c2, c3) => println(s"$c2:$c3 in $c1") }
//  }

    test("intervals ex 2.7") {
      println(ControlDataFlow.IntervalModule.lines().count())
      val fun = loadFunction(ControlDataFlow.IntervalModule)
      println(fun.compiled.ir.copy(scalaContent = Seq()).toString.lines().count())
      println(fun.compiled.optimized.copy(scalaContent = Seq()).toString.lines().count())
      println(fun.compiled.psystemSource.toString.lines().count())
}

  //  test("intervals ex 2.7") {
//    val fun = loadFunction(ControlDataFlow.IntervalModule)
//
//    // fun.compiled.printStatistics()
//    val prog = fun.input(ControlDataFlow.example_2_7)
//
//    val rels = fun.compiled.optimized.pats
//    println("relations: " + rels.size)
//    println("input relations: " + rels.count(_.name.contains(demandPatternPrefix)))
//    println("bodies: " + rels.flatMap(_.bodies).size)
//    println("atoms: " + rels.flatMap(_.bodies.flatMap(_.atoms)).size)
//
//    val res = fun.executeTuple("final_var", Tuples.flatTupleOf(prog))
////    fun.printAllMatches()
//    // TODO second aggregation currently implemented by hand in exit_var_external, should be generated eventually
//    assert(res.res.size == 2)
//  }

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

  test("powerset dataflow analysis") {
    val fun = loadFunction(ControlDataFlow.IntValuesModule)
    val input = fun.input(Seq(ControlDataFlow.exampleDataflow))
    assert(fun.executeInput("final_var", input).res.size == 100)
  }
}
