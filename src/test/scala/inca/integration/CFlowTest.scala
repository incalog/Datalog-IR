package inca.integration

import inca.Executor._
import inca.examples.CFlow
import org.scalatest.funsuite.AnyFunSuite

class CFlowTest extends AnyFunSuite {

  test("Plus Example") {
    val fun = loadFunction(CFlow.flowModule)
    assert(fun.execute("flow_bff", Seq(CFlow.example_2_1)).res.size == 4)
    fun.printAllMatches()
  }
}
