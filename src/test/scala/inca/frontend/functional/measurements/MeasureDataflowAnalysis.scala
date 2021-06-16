package inca.frontend.functional.measurements

import inca.examples.functional.ControlDataFlow
import inca.frontend.functional.executor.FunctionalExecutor.loadFunction
import org.fusesource.jansi.AnsiRenderer.test
import org.scalatest.funsuite.AnyFunSuite

class MeasureDataflowAnalysis extends AnyFunSuite {

  test("Dataflow Analysis") {
    val fun = loadFunction(ControlDataFlow.IntValuesModule)
    val input = fun.input(Seq(ControlDataFlow.exampleDataflow))

    fun.executeInput("final_var", input)
    fun.printAllMatches()
    fun.output("final_var", input._2).res.foreach { case Seq(c1, c2) => println(s"$c1 in $c2") }
  }
}
