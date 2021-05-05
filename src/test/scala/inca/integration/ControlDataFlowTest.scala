package inca.integration

import inca.executor.FunctionalExecutor._
import inca.examples.functional.ControlDataFlow
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples
import org.scalatest.funsuite.AnyFunSuite

class ControlDataFlowTest extends AnyFunSuite {

  test("flow ex 2.1") {
    val fun = loadFunction(ControlDataFlow.cflowModule)
    val input = fun.input(Seq(ControlDataFlow.example_2_1))
    assert(fun.executeTuple("flow", input).res.size == 4)
    assert(fun.output("flowR", input).res.isEmpty)
    fun.printAllMatches()
  }

  test("flowR ex 2.1") {
    val fun = loadFunction(ControlDataFlow.cflowModule)
    val input = fun.input(Seq(ControlDataFlow.example_2_1))
    assert(fun.executeTuple("flowR", input).res.size == 4)
    assert(fun.output("flow", input).res.size == 4)
    fun.printAllMatches()
  }

  test("transitiveFlow ex 2.1") {
    val fun = loadFunction(ControlDataFlow.cflowModule)
    val input = fun.input(Seq(ControlDataFlow.example_2_1))
    assert(fun.executeTuple("transitiveFlow", input).res.size == 12)
    fun.printAllMatches()
  }

//  test("available expressions ex 2.4") {
//    val fun = loadFunction(ControlDataFlow.AEModule)
//    val prog = fun.input(ControlDataFlow.example_2_4)
//    assert(fun.executeTuple("final_AE", Tuples.flatTupleOf(prog)).res.size == 1)
//    assert(fun.executeTuple("allEntries_AE", Tuples.flatTupleOf(prog)).res.size == 3)
//    assert(fun.executeTuple("allExits_AE", Tuples.flatTupleOf(prog)).res.size == 5)
//    fun.printAllMatches()
//    fun.output("allExits_AE", Tuples.flatTupleOf(prog)).res.foreach { case Seq(c1, c2) => println(s"$c2 in $c1") }
//  }

  test("reaching definitions ex 2.7") {
    val fun = loadFunction(ControlDataFlow.RDmodule)
    val prog = fun.input(ControlDataFlow.example_2_7)

    val rels = fun.compiled.optimized.pats.filter(!_.name.contains("coal"))
    println("relations: " + rels.size)
    println("input relations: " + rels.filter(_.name.contains("input_")).size)
    println("bodies: " + rels.flatMap(_.bodies).size)
    println("constraints: " + rels.flatMap(_.bodies.flatMap(_.constraints)).size)

    assert(fun.executeTuple("final_RD", Tuples.flatTupleOf(prog)).res.size == 4)
    assert(fun.executeTuple("allEntries_RD", Tuples.flatTupleOf(prog)).res.size == 15)
    assert(fun.executeTuple("allExits_RD", Tuples.flatTupleOf(prog)).res.size == 13)

    fun.printAllMatches()
    fun.output("allExits_RD", Tuples.flatTupleOf(prog)).res.foreach { case Seq(c1, c2, c3) => println(s"$c2:$c3 in $c1") }
  }

  test("intervals ex 2.7") {
    val fun = loadFunction(ControlDataFlow.IntervalModule)
    // fun.compiled.printStatistics()
    val prog = fun.input(ControlDataFlow.example_2_7)

    val rels = fun.compiled.optimized.pats.filter(!_.name.contains("coal"))
    println("relations: " + rels.size)
    println("input relations: " + rels.filter(_.name.contains("input_")).size)
    println("bodies: " + rels.flatMap(_.bodies).size)
    println("constraints: " + rels.flatMap(_.bodies.flatMap(_.constraints)).size)

    val res = fun.executeTuple("final_var", Tuples.flatTupleOf(prog))
    fun.printAllMatches()
    // TODO second aggregation currently implemented by hand in exit_var_external, should be generated eventually
    assert(res.res.size == 2)
  }

  test("aeval") {
    val fun = loadFunction(ControlDataFlow.AEvalModule)
    fun.compiled.printStatistics()
    val names = Set("aeval", "add","sub","greaterThan","mul")
    val m = fun.compiled.optimized.copy(pats = fun.compiled.optimized.pats.filter(p => names.exists(n => p.name.contains(n))), scalaContent = Seq())
    println(m)
    println(s"GP relations: ${m.pats.size}")
    println(s"GP bodies: ${m.pats.map(_.bodies.size).sum}")
//    val prog = fun.input(ControlDataFlow.example_2_7)
//
//    val rels = fun.compiled.optimized.pats.filter(!_.name.contains("coal"))
//    println("relations: " + rels.size)
//    println("input relations: " + rels.filter(_.name.contains("input_")).size)
//    println("bodies: " + rels.flatMap(_.bodies).size)
//    println("constraints: " + rels.flatMap(_.bodies.flatMap(_.constraints)).size)
//
//    val res = fun.executeTuple("final_var", Tuples.flatTupleOf(prog))
//    fun.printAllMatches()
//    // TODO second aggregation currently implemented by hand in exit_var_external, should be generated eventually
//    assert(res.res.size == 2)
  }
}
