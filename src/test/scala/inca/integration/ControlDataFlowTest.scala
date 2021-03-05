package inca.integration

import inca.Executor._
import inca.examples.ControlDataFlow
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples
import org.scalatest.funsuite.AnyFunSuite

class ControlDataFlowTest extends AnyFunSuite {

  test("flow ex 2.1") {
    val fun = loadFunction(ControlDataFlow.cflowModule)
    val input = fun.input(Seq(ControlDataFlow.example_2_1))
    assert(fun.executeTuple("flow_bff", input, deleteInput = false).res.size == 4)
    assert(fun.output("flowR_bff", input).res.isEmpty)
    fun.printAllMatches()
  }

  test("flowR ex 2.1") {
    val fun = loadFunction(ControlDataFlow.cflowModule)
    val input = fun.input(Seq(ControlDataFlow.example_2_1))
    assert(fun.executeTuple("flowR_bff", input, deleteInput = false).res.size == 4)
    assert(fun.output("flow_bff", input).res.size == 4)
    fun.printAllMatches()
  }

  test("available expressions ex 2.4") {
    val fun = loadFunction(ControlDataFlow.AEModule)
    val prog = fun.input(ControlDataFlow.example_2_4)
    assert(fun.executeTuple("final_AE_bf", Tuples.flatTupleOf(prog), deleteInput = false).res.size == 1)
    assert(fun.executeTuple("allEntries_AE_bff", Tuples.flatTupleOf(prog), deleteInput = false).res.size == 3)
    assert(fun.executeTuple("allExits_AE_bff", Tuples.flatTupleOf(prog), deleteInput = false).res.size == 5)
    fun.printAllMatches()
    fun.output("allExits_AE_bff", Tuples.flatTupleOf(prog)).res.foreach { case Seq(c1, c2) => println(s"$c2 in $c1") }
  }

  test("reaching definitions ex 2.7") {
    val fun = loadFunction(ControlDataFlow.RDmodule)
    val prog = fun.input(ControlDataFlow.example_2_7)
    assert(fun.executeTuple("final_RD_bff", Tuples.flatTupleOf(prog)).res.size == 4)
    assert(fun.executeTuple("allEntries_RD_bfff", Tuples.flatTupleOf(prog)).res.size == 15)
    assert(fun.executeTuple("allExits_RD_bfff", Tuples.flatTupleOf(prog), deleteInput = false).res.size == 13)
    fun.printAllMatches()
    fun.output("allExits_RD_bfff", Tuples.flatTupleOf(prog)).res.foreach { case Seq(c1, c2, c3) => println(s"$c2:$c3 in $c1") }
  }

  test("intervals ex 2.7") {
    val fun = loadFunction(ControlDataFlow.IntervalModule)
    val prog = fun.input(ControlDataFlow.example_2_7)
    val res = fun.executeTuple("final_var_bff", Tuples.flatTupleOf(prog))
    fun.printAllMatches()
    // TODO second aggregation currently implemented by hand in exit_var_external, should be generated eventually
    assert(res.res.size == 2)
  }
}
