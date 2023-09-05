package inca.frontend.functional.integration

import inca.examples.functional.ControlDataFlow
import inca.frontend.functional.executor.FunctionalExecutor._
import inca.runtime.EnginePool
import inca.util.FileUtil
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.BeforeAndAfterEach

class ControlDataFlowTest extends AnyFunSuite with BeforeAndAfterEach {

  override def afterEach(): Unit = {
    EnginePool.disposeAllEngines()
  }

  test("flow ex 2.1") {
    val code = FileUtil.readFile("functional/controlflow/CFlow.finca")
    val fun = loadFunction(code)
    val input = fun.input(Seq(ControlDataFlow.example_2_1))
    assert(fun.executeInput("flow", input).res.size == 4)
    assert(fun.output("flowR", input._2).res.isEmpty)
  }

  test("flowR ex 2.1") {
    val code = FileUtil.readFile("functional/controlflow/CFlow.finca")
    val fun = loadFunction(code)
    val input = fun.input(Seq(ControlDataFlow.example_2_1))
    assert(fun.executeInput("flowR", input).res.size == 4)
    assert(fun.output("flow", input._2).res.size == 4)
  }

  test("transitiveFlow ex 2.1") {
    val code = FileUtil.readFile("functional/controlflow/CFlow.finca")
    val fun = loadFunction(code)
    val input = fun.input(Seq(ControlDataFlow.example_2_1))
    assert(fun.executeInput("transitiveFlow", input).res.size == 12)
  }

  ignore("available expressions ex 2.4") {
    val code = FileUtil.readFile("functional/controlflow/AvailableExpressions.finca")
    val fun = loadFunction(code)
    val input = fun.input(ControlDataFlow.example_2_4)
    assert(fun.executeInput("final_AE", input).res.size == 1)
    assert(fun.executeInput("allEntries_AE", input).res.size == 3)
    assert(fun.executeInput("allExits_AE", input).res.size == 5)
    fun.printAllMatches()
    fun.output("allExits_AE", input._2).res.foreach { case Seq(c1, c2) => println(s"$c2 in $c1") }
  }

  // TODO fix bug. Duplicate insertion
  ignore("reaching definitions ex 2.7") {
    val code = FileUtil.readFile("functional/controlflow/ReachingDefinition.finca")
    val fun = loadFunction(code)
    val input = fun.input(ControlDataFlow.example_2_7)

    assert(fun.executeInput("final_RD", input).res.size == 4)
    assert(fun.executeInput("allEntries_RD", input).res.size == 15)
    assert(fun.executeInput("allExits_RD", input).res.size == 13)

    fun.output("allExits_RD", input._2).res.foreach { case Seq(c1, c2, c3) =>
      println(s"$c2:$c3 in $c1")
    }
  }

  test("intervals ex 2.7") {
    val code = FileUtil.readFile("functional/controlflow/Interval.finca")
    val fun = loadFunction(code)

    val prog = fun.input(ControlDataFlow.example_2_7)
    val res = fun.executeInput("final_var", prog)
    // TODO second aggregation currently implemented by hand in exit_var_external, should be generated eventually
    assert(res.res.size == 2)
  }

  test("aeval") {
    val code = FileUtil.readFile("functional/controlflow/AEval.finca")
    val fun = loadFunction(code)
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
