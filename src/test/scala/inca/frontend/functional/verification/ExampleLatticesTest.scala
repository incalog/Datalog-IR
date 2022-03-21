package inca.frontend.functional.verification

import inca.frontend.functional.core.{Associativity, Commutativity}
import inca.frontend.functional.verification.examples.Aggregations.{compiledDoubleOperationsModule, compiledIntegerOperationsModule, compiledStringOperationsModule}
import org.scalatest.funsuite.AnyFunSuite
import inca.frontend.functional.verification.examples.Lattices.{compiledBoolLattice, compiledConstLattice, compiledIntervalLattice, compiledIntervalLatticeInvariants, compiledModifiedIntervalLattice, compiledSignLattice, compiledSignValLattice}


class ExampleLatticesTest extends AnyFunSuite {
  test("test sign lattice verification") {
    val module = compiledSignLattice.typed
    val verifier = new Verifier()
    assertResult(Map(
      "join$0" -> Map(Associativity -> SatisfiedResponse, Commutativity -> SatisfiedResponse)
    ))(verifier.verify(module))
  }

  test("test const lattice verification") {
    val module = compiledConstLattice.typed
    val verifier = new Verifier()
    assertResult(Map(
      "join$0" -> Map(Associativity -> SatisfiedResponse, Commutativity -> SatisfiedResponse)
    ))(verifier.verify(module))
  }

  test("test signVal lattice verification") {
    val module = compiledSignValLattice.typed
    val verifier = new Verifier()
    assertResult(Map(
      "join$0" -> Map(Associativity -> SatisfiedResponse, Commutativity -> SatisfiedResponse),
      "joinBool$0" -> Map(Associativity -> SatisfiedResponse, Commutativity -> SatisfiedResponse),
      "joinSign$0" -> Map(Associativity -> SatisfiedResponse, Commutativity -> SatisfiedResponse)
    ))(verifier.verify(module))
  }

  test("test integerOperations module verification") {
    val module = compiledIntegerOperationsModule.typed
    val verifier = new Verifier()
    assertResult(Map(
      "add$0" -> Map(Associativity -> SatisfiedResponse, Commutativity -> SatisfiedResponse),
      "sub$0" -> Map(Associativity -> UnsatisfiedResponse, Commutativity -> UnsatisfiedResponse),
      "mult$0" -> Map(Associativity -> SatisfiedResponse, Commutativity -> SatisfiedResponse),
      "div$0" -> Map(Associativity -> UnsatisfiedResponse, Commutativity -> UnsatisfiedResponse),
      "min$0" -> Map(Associativity -> SatisfiedResponse, Commutativity -> SatisfiedResponse),
      "pow$0" -> Map(Associativity -> UnsatisfiedResponse, Commutativity -> UnsatisfiedResponse),
    ))(verifier.verify(module))
  }

  test("test doubleOperations module verification") {
    val module = compiledDoubleOperationsModule.typed
    val verifier = new Verifier()
    assertResult(Map(
      "add$0" -> Map(Associativity -> SatisfiedResponse, Commutativity -> SatisfiedResponse),
      "sub$0" -> Map(Associativity -> UnsatisfiedResponse, Commutativity -> UnsatisfiedResponse),
      "mult$0" -> Map(Associativity -> SatisfiedResponse, Commutativity -> SatisfiedResponse),
      "div$0" -> Map(Associativity -> UnsatisfiedResponse, Commutativity -> UnsatisfiedResponse),
      "min$0" -> Map(Associativity -> SatisfiedResponse, Commutativity -> SatisfiedResponse),
    ))(verifier.verify(module))
  }

  test("test stringOperations module verification") {
    val module = compiledStringOperationsModule.typed
    val verifier = new Verifier()
    assertResult(Map(
      "concat$0" -> Map(Associativity -> SatisfiedResponse, Commutativity -> UnsatisfiedResponse),
    ))(verifier.verify(module))
  }

  // joinInterval ist nicht assoziativ, solange man Intervalle erstellen kann mit l > h
  test("test interval module verification") {
    val module = compiledIntervalLattice.typed
    val verifier = new Verifier()
    assertResult(Map(
      "joinVal$0" -> Map(Associativity -> UnsatisfiedResponse, Commutativity -> SatisfiedResponse),
      "joinBool$0" -> Map(Associativity -> SatisfiedResponse, Commutativity -> SatisfiedResponse),
      "joinInterval$0" -> Map(Associativity -> UnsatisfiedResponse, Commutativity -> SatisfiedResponse)
    ))(verifier.verify(module))
  }

  test("test modified interval module verification") {
    val module = compiledModifiedIntervalLattice.typed
    val verifier = new Verifier()
    assertResult(Map(
      "joinVal$0" -> Map(Associativity -> SatisfiedResponse, Commutativity -> SatisfiedResponse),
      "joinBool$0" -> Map(Associativity -> SatisfiedResponse, Commutativity -> SatisfiedResponse),
      "joinInterval$0" -> Map(Associativity -> SatisfiedResponse, Commutativity -> SatisfiedResponse)
    ))(verifier.verify(module))
  }

  test("test interval module with invariants verification") {
    val module = compiledIntervalLatticeInvariants.typed
    val verifier = new Verifier()
    assertResult(Map(
      "joinVal$0" -> Map(Associativity -> SatisfiedResponse, Commutativity -> SatisfiedResponse),
      "joinBool$0" -> Map(Associativity -> SatisfiedResponse, Commutativity -> SatisfiedResponse),
      "joinInterval$0" -> Map(Associativity -> SatisfiedResponse, Commutativity -> SatisfiedResponse)
    ))(verifier.verify(module))
  }

  // funktioniert nicht, da Bool ein protected word ist in z3
  test("test bool lattice module verification") {
    val module = compiledBoolLattice.typed
    val verifier = new Verifier()
    assertResult(Map(
      "joinBool$0" -> Map(Associativity -> SatisfiedResponse, Commutativity -> SatisfiedResponse)
    ))(verifier.verify(module))
  }
}
