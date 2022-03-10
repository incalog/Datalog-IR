package inca.frontend.functional.verification

import inca.examples.functional.ControlDataFlow.IntervalModule
import inca.frontend.functional.core.{Associativity, Commutativity}
import inca.frontend.functional.parser.Parser
import inca.frontend.functional.verification.examples.Aggregations.compiledAdditionModule
import org.scalatest.funsuite.AnyFunSuite
import inca.frontend.functional.verification.examples.Lattices.{compiledConstLattice, compiledIntervalLattice, compiledSignLattice, compiledSignValLattice}


class ExampleLatticesTest extends AnyFunSuite {
  test("test sign lattice verification") {
    val module = compiledSignLattice.typed
    val verifier = new Verifier()
    assertResult(Map("join" -> Map(Associativity -> SatisfiedResponse, Commutativity -> SatisfiedResponse)))(verifier.verify(module))
  }

  test("test const lattice verification") {
    val module = compiledConstLattice.typed
    val verifier = new Verifier()
    assertResult(Map("join" -> Map(Associativity -> SatisfiedResponse, Commutativity -> SatisfiedResponse)))(verifier.verify(module))
  }

  test("test signVal lattice verification") {
    val module = compiledSignValLattice.typed
    val verifier = new Verifier()
    assertResult(Map("join" -> Map(Associativity -> SatisfiedResponse, Commutativity -> SatisfiedResponse),
      "joinBool" -> Map(Associativity -> SatisfiedResponse, Commutativity -> SatisfiedResponse),
      "joinSign" -> Map(Associativity -> SatisfiedResponse, Commutativity -> SatisfiedResponse)))(verifier.verify(module))
  }

  test("test addition module verification") {
    val module = compiledAdditionModule.typed
    val verifier = new Verifier()
    assertResult(Map("add" -> Map(Associativity -> SatisfiedResponse, Commutativity -> SatisfiedResponse),
      "sub" -> Map(Associativity -> UnsatisfiedResponse, Commutativity -> UnsatisfiedResponse)))(verifier.verify(module))
  }

  test("test interval module verification") {
    val module = compiledIntervalLattice.typed
    val verifier = new Verifier()
    assertResult(Map("joinVal" -> Map(Associativity -> SatisfiedResponse, Commutativity -> SatisfiedResponse),
      "joinBool" -> Map(Associativity -> SatisfiedResponse, Commutativity -> SatisfiedResponse),
      "joinInterval" -> Map(Associativity -> SatisfiedResponse, Commutativity -> SatisfiedResponse)))(verifier.verify(module))
  }
}
