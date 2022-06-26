package inca.frontend.functional.verification

import inca.frontend.functional.core.{Associativity, Commutativity, HasUnapply, SoundnessAnno}
import inca.frontend.functional.verification.examples.Aggregations.{compiledDoubleOperationsModule, compiledIntegerOperationsModule, compiledStringOperationsModule}
import org.scalatest.funsuite.AnyFunSuite
import inca.frontend.functional.verification.examples.Lattices.{compiledBoolLattice, compiledConstLattice, compiledIntervalLattice, compiledIntervalLatticeInvariants, compiledModifiedIntervalLattice, compiledSignLattice, compiledSignLatticeWithPartialOrder, compiledSignValLattice}


class ExampleLatticesTest extends AnyFunSuite {
  test("test sign lattice verification") {
    val module = compiledSignLattice.typed
    val verifier = new Verifier()
    assertResult(Map(
      "join" -> Map(Left(Associativity) -> VerifiedResponse, Left(Commutativity) -> VerifiedResponse)
    ))(verifier.verify(module))
  }

  test("test sign lattice with chooseLeft verification") {
    val module = compiledSignLatticeWithPartialOrder.typed
    val verifier = new Verifier()
    assertResult(Map(
      "join" -> Map(Left(Associativity) -> VerifiedResponse, Left(Commutativity) -> VerifiedResponse,
        Right(SoundnessAnno("chooseLeft", "intToSign", "intToSign", "leq")) -> VerifiedResponse)
    ))(verifier.verify(module))
  }

  test("test const lattice verification") {
    val module = compiledConstLattice.typed
    val verifier = new Verifier()
    assertResult(Map(
      "join" -> Map(Left(Associativity) -> VerifiedResponse, Left(Commutativity) -> VerifiedResponse)
    ))(verifier.verify(module))
  }

  test("test signVal lattice verification") {
    val module = compiledSignValLattice.typed
    val verifier = new Verifier()
    assertResult(Map(
      "join" -> Map(Left(Associativity) -> VerifiedResponse, Left(Commutativity) -> VerifiedResponse),
      "joinBool" -> Map(Left(Associativity) -> VerifiedResponse, Left(Commutativity) -> VerifiedResponse),
      "joinSign" -> Map(Left(Associativity) -> VerifiedResponse, Left(Commutativity) -> VerifiedResponse)
    ))(verifier.verify(module))
  }

  test("test integerOperations module verification") {
    val module = compiledIntegerOperationsModule.typed
    val verifier = new Verifier()
    assertResult(Map(
      "add" -> Map(Left(Associativity) -> VerifiedResponse, Left(Commutativity) -> VerifiedResponse, Left(HasUnapply("sub")) -> VerifiedResponse),
      "sub" -> Map(Left(Associativity) -> FalsifiedResponse, Left(Commutativity) -> FalsifiedResponse, Left(HasUnapply("add")) -> VerifiedResponse),
      // mult, div nicht invertierbar, weil Mult mit 0 und Div durch 0 nicht umkehrbar
      "mult" -> Map(Left(Associativity) -> VerifiedResponse, Left(Commutativity) -> VerifiedResponse, Left(HasUnapply("div")) -> FalsifiedResponse),
      "div" -> Map(Left(Associativity) -> FalsifiedResponse, Left(Commutativity) -> FalsifiedResponse, Left(HasUnapply("mult")) -> FalsifiedResponse),
      "incByTwo" -> Map(Left(Associativity) -> VerifiedResponse, Left(Commutativity) -> VerifiedResponse),
      "min" -> Map(Left(Associativity) -> VerifiedResponse, Left(Commutativity) -> VerifiedResponse),
      "pow" -> Map(Left(Associativity) -> FalsifiedResponse, Left(Commutativity) -> FalsifiedResponse),
    ))(verifier.verify(module))
  }

  test("test doubleOperations module verification") {
    val module = compiledDoubleOperationsModule.typed
    val verifier = new Verifier()
    assertResult(Map(
      "add" -> Map(Left(Associativity) -> VerifiedResponse, Left(Commutativity) -> VerifiedResponse, Left(HasUnapply("sub")) -> VerifiedResponse),
      "sub" -> Map(Left(Associativity) -> FalsifiedResponse, Left(Commutativity) -> FalsifiedResponse, Left(HasUnapply("add")) -> VerifiedResponse),
      "mult" -> Map(Left(Associativity) -> VerifiedResponse, Left(Commutativity) -> VerifiedResponse, Left(HasUnapply("div")) -> FalsifiedResponse),
      "div" -> Map(Left(Associativity) -> FalsifiedResponse, Left(Commutativity) -> FalsifiedResponse, Left(HasUnapply("mult")) -> FalsifiedResponse),
      "min" -> Map(Left(Associativity) -> VerifiedResponse, Left(Commutativity) -> VerifiedResponse),
    ))(verifier.verify(module))
  }

  /*
    test("test nonZeroDoubles module verification") {
    val module = compiledNonZeroDoublesModule.typed
    val verifier = new Verifier()
    assertResult(Map(
      "add" -> Map(Left(Associativity) -> VerifiedResponse, Left(Commutativity) -> VerifiedResponse, Left(HasUnapply("sub")) -> VerifiedResponse),
      "sub" -> Map(Left(Associativity) -> FalsifiedResponse, Left(Commutativity) -> FalsifiedResponse, Left(HasUnapply("add")) -> VerifiedResponse),
      "mult" -> Map(Left(Associativity) -> VerifiedResponse, Left(Commutativity) -> VerifiedResponse, Left(HasUnapply("div")) -> VerifiedResponse),
      "div" -> Map(Left(Associativity) -> FalsifiedResponse, Left(Commutativity) -> FalsifiedResponse, Left(HasUnapply("mult")) -> VerifiedResponse),
      "min" -> Map(Left(Associativity) -> VerifiedResponse, Left(Commutativity) -> VerifiedResponse),
    ))(verifier.verify(module))
  }
  */

  test("test stringOperations module verification") {
    val module = compiledStringOperationsModule.typed
    val verifier = new Verifier()
    assertResult(Map(
      "concat" -> Map(Left(Associativity) -> VerifiedResponse, Left(Commutativity) -> FalsifiedResponse),
    ))(verifier.verify(module))
  }

  // joinInterval ist nicht assoziativ, solange man Intervalle erstellen kann mit l > h
  test("test interval module verification") {
    val module = compiledIntervalLattice.typed
    val verifier = new Verifier()
    assertResult(Map(
      "joinVal" -> Map(Left(Associativity) -> FalsifiedResponse, Left(Commutativity) -> VerifiedResponse),
      "joinBool" -> Map(Left(Associativity) -> VerifiedResponse, Left(Commutativity) -> VerifiedResponse),
      "joinInterval" -> Map(Left(Associativity) -> FalsifiedResponse, Left(Commutativity) -> VerifiedResponse)
    ))(verifier.verify(module))
  }

  test("test modified interval module verification") {
    val module = compiledModifiedIntervalLattice.typed
    val verifier = new Verifier()
    assertResult(Map(
      "joinVal" -> Map(Left(Associativity) -> VerifiedResponse, Left(Commutativity) -> VerifiedResponse),
      "joinBool" -> Map(Left(Associativity) -> VerifiedResponse, Left(Commutativity) -> VerifiedResponse),
      "joinInterval" -> Map(Left(Associativity) -> VerifiedResponse, Left(Commutativity) -> VerifiedResponse)
    ))(verifier.verify(module))
  }

  test("test interval module with invariants verification") {
    val module = compiledIntervalLatticeInvariants.typed
    val verifier = new Verifier()
    assertResult(Map(
      "joinVal" -> Map(Left(Associativity) -> VerifiedResponse, Left(Commutativity) -> VerifiedResponse),
      "joinBool" -> Map(Left(Associativity) -> VerifiedResponse, Left(Commutativity) -> VerifiedResponse),
      "joinInterval" -> Map(Left(Associativity) -> VerifiedResponse, Left(Commutativity) -> VerifiedResponse)
    ))(verifier.verify(module))
  }

  test("test bool lattice module verification") {
    val module = compiledBoolLattice.typed
    val verifier = new Verifier()
    assertResult(Map(
      "joinBool" -> Map(Left(Associativity) -> VerifiedResponse, Left(Commutativity) -> VerifiedResponse)
    ))(verifier.verify(module))
  }
}
