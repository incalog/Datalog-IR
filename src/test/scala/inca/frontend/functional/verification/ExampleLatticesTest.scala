package inca.frontend.functional.verification

import inca.examples.functional.ControlDataFlow.IntervalModule
import inca.frontend.functional.core.{Associativity, Commutativity}
import inca.frontend.functional.parser.Parser
import inca.frontend.functional.verification.examples.Aggregations.addition_module
import org.scalatest.funsuite.AnyFunSuite
import inca.frontend.functional.verification.examples.Lattices.{const_lattice_module, interval_lattice_module, signVal_lattice_module, sign_lattice, sign_lattice_module}


class ExampleLatticesTest extends AnyFunSuite {
  test("test sign_lattice_module verification") {
    val module = sign_lattice_module
    val verifier = new Verifier()
    assertResult(Map("join" -> Map(Associativity -> SatisfiedResponse, Commutativity -> SatisfiedResponse)))(verifier.verify(module))
  }

  test("test const_lattice_module verification") {
    val module = const_lattice_module
    val verifier = new Verifier()
    assertResult(Map("join" -> Map(Associativity -> SatisfiedResponse, Commutativity -> SatisfiedResponse)))(verifier.verify(module))
  }

  test("test signVal_lattice_module verification") {
    val module = signVal_lattice_module
    val verifier = new Verifier()
    assertResult(Map("join" -> Map(Associativity -> SatisfiedResponse, Commutativity -> SatisfiedResponse),
      "joinBool" -> Map(Associativity -> SatisfiedResponse, Commutativity -> SatisfiedResponse),
      "joinSign" -> Map(Associativity -> SatisfiedResponse, Commutativity -> SatisfiedResponse)))(verifier.verify(module))
  }

  test("test addition_module verification") {
    val module = addition_module
    val verifier = new Verifier()
    assertResult(Map("add" -> Map(Associativity -> SatisfiedResponse, Commutativity -> SatisfiedResponse),
      "sub" -> Map(Associativity -> UnsatisfiedResponse, Commutativity -> UnsatisfiedResponse)))(verifier.verify(module))
  }

  test("test IntervalModule verification") {
    val module = interval_lattice_module
    val verifier = new Verifier()
    assertResult(Map("joinVal" -> Map(Associativity -> SatisfiedResponse, Commutativity -> SatisfiedResponse),
      "joinBool" -> Map(Associativity -> SatisfiedResponse, Commutativity -> SatisfiedResponse),
      "joinInterval" -> Map(Associativity -> SatisfiedResponse, Commutativity -> SatisfiedResponse)))(verifier.verify(module))
  }
}
