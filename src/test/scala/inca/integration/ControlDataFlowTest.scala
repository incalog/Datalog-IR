package inca.integration

import inca.Executor._
import inca.examples.ControlDataFlow
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples
import org.scalatest.funsuite.AnyFunSuite

class ControlDataFlowTest extends AnyFunSuite {

  test("flow ex 2.1") {
    val fun = loadFunction(ControlDataFlow.cflowModule)
    val input = fun.input(Seq(ControlDataFlow.example_2_1))
    assert(fun.execute("flow_bff", input, deleteInput = false).res.size == 4)
    assert(fun.output("flowR_bff", input).res.isEmpty)
    fun.printAllMatches()
  }

  test("flowR ex 2.1") {
    val fun = loadFunction(ControlDataFlow.cflowModule)
    val input = fun.input(Seq(ControlDataFlow.example_2_1))
    assert(fun.execute("flowR_bff", input, deleteInput = false).res.size == 4)
    assert(fun.output("flow_bff", input).res.size == 4)
    fun.printAllMatches()
  }

  test("reaching definitions ex 2.7 -- assignments") {
    import scala.meta.XtensionQuasiquoteTerm
    val fun = loadFunction(ControlDataFlow.RDmodule)
    val prog = fun.input(ControlDataFlow.example_2_7)
    assert(fun.execute("assignments_bbf", Tuples.flatTupleOf(prog, fun.input(q""" "x" """)), deleteInput = true).res.size == 2)
    assert(fun.execute("assignments_bbf", Tuples.flatTupleOf(prog, fun.input(q""" "y" """)), deleteInput = false).res.size == 2)
    fun.printAllMatches()
  }

  test("reaching definitions ex 2.7 -- killAll") {
    val fun = loadFunction(ControlDataFlow.RDmodule)
    val prog = fun.input(ControlDataFlow.example_2_7)
    assert(fun.execute("killAll_RD_bfff", Tuples.flatTupleOf(prog), deleteInput = false).res.size == 12)
    fun.printAllMatches()
  }

  test("reaching definitions ex 2.7 -- genAll") {
    val fun = loadFunction(ControlDataFlow.RDmodule)
    val prog = fun.input(ControlDataFlow.example_2_7)
    assert(fun.execute("genAll_RD_bfff", Tuples.flatTupleOf(prog), deleteInput = false).res.size == 4)
    fun.printAllMatches()
  }

  test("reaching definitions ex 2.7 -- flow") {
    val fun = loadFunction(ControlDataFlow.RDmodule)
    val prog = fun.input(ControlDataFlow.example_2_7)
    assert(fun.execute("flow_bff", Tuples.flatTupleOf(prog), deleteInput = false).res.size == 5)
    fun.printAllMatches()
  }

  test("reaching definitions ex 2.7 -- freevarsStm") {
    val fun = loadFunction(ControlDataFlow.RDmodule)
    val prog = fun.input(ControlDataFlow.example_2_7)
    assert(fun.execute("freevarsStm_bf", Tuples.flatTupleOf(prog), deleteInput = false).res.size == 2)
    fun.printAllMatches()
  }

  test("reaching definitions ex 2.7 -- entryAll") {
    import scala.meta._
    val fun = loadFunction(ControlDataFlow.RDmodule)
    println(fun.execute("entryAll_RD_bfff", Seq(q"""Sequence(Assign("x", Var("y")), Assign("w", Var("x")))""")))
//    assert(fun.execute("entryAll_RD_bfff", Seq(q"""Sequence(Assign("x", Var("y")), Assign("w", Var("x")))"""), deleteInput = false).res.size == 3)
//    val prog = fun.input(ControlDataFlow.example_2_7)
//    assert(fun.execute("entryAll_RD_bfff", Tuples.flatTupleOf(prog), deleteInput = false).res.size == 15)
    fun.printAllMatches()
  }
}
