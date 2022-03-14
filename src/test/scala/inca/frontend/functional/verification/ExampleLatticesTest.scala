package inca.frontend.functional.verification

import inca.examples.functional.ControlDataFlow.IntervalModule
import inca.frontend.functional.core.{Associativity, Commutativity}
import inca.frontend.functional.parser.Parser
import inca.frontend.functional.verification.examples.Aggregations.{compiledIntegerOperationsModule, compiledStringOperationsModule}
import org.scalatest.funsuite.AnyFunSuite
import inca.frontend.functional.verification.examples.Lattices.{compiledConstLattice, compiledIntervalLattice, compiledSignLattice, compiledSignValLattice}


class ExampleLatticesTest extends AnyFunSuite {
  test("test sign lattice verification") {
    val module = compiledSignLattice.typed
    val verifier = new Verifier()
    assertResult(Map(
      "join" -> Map(Associativity -> SatisfiedResponse, Commutativity -> SatisfiedResponse)
    ))(verifier.verify(module))
  }

  test("test const lattice verification") {
    val module = compiledConstLattice.typed
    val verifier = new Verifier()
    assertResult(Map(
      "join" -> Map(Associativity -> SatisfiedResponse, Commutativity -> SatisfiedResponse)
    ))(verifier.verify(module))
  }

  test("test signVal lattice verification") {
    val module = compiledSignValLattice.typed
    val verifier = new Verifier()
    assertResult(Map(
      "join" -> Map(Associativity -> SatisfiedResponse, Commutativity -> SatisfiedResponse),
      "joinBool" -> Map(Associativity -> SatisfiedResponse, Commutativity -> SatisfiedResponse),
      "joinSign" -> Map(Associativity -> SatisfiedResponse, Commutativity -> SatisfiedResponse)
    ))(verifier.verify(module))
  }

  test("test integerOperations module verification") {
    val module = compiledIntegerOperationsModule.typed
    val verifier = new Verifier()
    assertResult(Map(
      "add" -> Map(Associativity -> SatisfiedResponse, Commutativity -> SatisfiedResponse),
      "sub" -> Map(Associativity -> UnsatisfiedResponse, Commutativity -> UnsatisfiedResponse),
      "mult" -> Map(Associativity -> SatisfiedResponse, Commutativity -> SatisfiedResponse),
      "min" -> Map(Associativity -> SatisfiedResponse, Commutativity -> SatisfiedResponse),
      "pow" -> Map(Associativity -> UnsatisfiedResponse, Commutativity -> UnsatisfiedResponse),
    ))(verifier.verify(module))
  }

  test("test stringOperations module verification") {
    val module = compiledStringOperationsModule.typed
    val verifier = new Verifier()
    assertResult(Map(
      "concat" -> Map(Associativity -> SatisfiedResponse, Commutativity -> UnsatisfiedResponse),
    ))(verifier.verify(module))
  }

  // joinInterval ist nicht assoziativ, solange man Intervalle erstellen kann mit l > h
  test("test interval module verification") {
    val module = compiledIntervalLattice.typed
    val verifier = new Verifier()
    assertResult(Map(
      "joinVal" -> Map(Associativity -> UnsatisfiedResponse, Commutativity -> SatisfiedResponse),
      "joinBool" -> Map(Associativity -> SatisfiedResponse, Commutativity -> SatisfiedResponse),
      "joinInterval" -> Map(Associativity -> UnsatisfiedResponse, Commutativity -> SatisfiedResponse)
    ))(verifier.verify(module))
  }
}
