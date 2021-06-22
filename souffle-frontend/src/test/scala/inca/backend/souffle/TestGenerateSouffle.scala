package inca.backend.souffle

import inca.examples.functional.ControlDataFlow
import org.scalatest.funsuite.AnyFunSuite


class TestGenerateSouffle extends AnyFunSuite {

  test("powerset dataflow analysis") {
    val compiled = CompiledFunctionalToSouffleModule(ControlDataFlow.IntValuesModule)
    println(compiled.souffleSource)
  }

}
