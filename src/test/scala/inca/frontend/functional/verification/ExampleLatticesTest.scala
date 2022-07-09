package inca.frontend.functional.verification

import inca.frontend.functional.core.{Associativity, Commutativity, HasUnapply, MonotonicityAnno, PartialOrderAnno, SoundnessAnno}
import inca.frontend.functional.verification.examples.Aggregations.{compiledDoubleOperationsModule, compiledIntegerOperationsModule, compiledStringOperationsModule}
import org.scalatest.funsuite.AnyFunSuite
import inca.frontend.functional.verification.examples.Lattices.{compiledBoolLattice, compiledConstLattice, compiledIntOpsSignLattice, compiledIntervalLattice, compiledIntervalLatticeInvariants, compiledModifiedIntervalLattice, compiledSignLattice, compiledSignValLattice}


class ExampleLatticesTest extends AnyFunSuite {
  test("test sign lattice verification") {
    val module = compiledSignLattice.typed
    val verifier = new Verifier()
    assertResult(Map(
      "join" -> Map(Associativity -> VerifiedResponse, Commutativity -> VerifiedResponse)
    ))(verifier.verify(module))
  }

  test("test const lattice verification") {
    val module = compiledConstLattice.typed
    val verifier = new Verifier()
    assertResult(Map(
      "join" -> Map(Associativity -> VerifiedResponse, Commutativity -> VerifiedResponse)
    ))(verifier.verify(module))
  }

  test("test signVal lattice verification") {
    val module = compiledSignValLattice.typed
    val verifier = new Verifier()
    assertResult(Map(
      "join" -> Map(Associativity -> VerifiedResponse, Commutativity -> VerifiedResponse),
      "joinBool" -> Map(Associativity -> VerifiedResponse, Commutativity -> VerifiedResponse),
      "joinSign" -> Map(Associativity -> VerifiedResponse, Commutativity -> VerifiedResponse)
    ))(verifier.verify(module))
  }

  test("test integerOperations module verification") {
    val module = compiledIntegerOperationsModule.typed
    val verifier = new Verifier()
    assertResult(Map(
      "add" -> Map(Associativity -> VerifiedResponse, Commutativity -> VerifiedResponse, HasUnapply("sub") -> VerifiedResponse),
      "sub" -> Map(Associativity -> FalsifiedResponse, Commutativity -> FalsifiedResponse, HasUnapply("add") -> VerifiedResponse),
      // mult, div nicht invertierbar, weil Mult mit 0 und Div durch 0 nicht umkehrbar
      "mult" -> Map(Associativity -> VerifiedResponse, Commutativity -> VerifiedResponse, HasUnapply("div") -> FalsifiedResponse),
      "div" -> Map(Associativity -> FalsifiedResponse, Commutativity -> FalsifiedResponse, HasUnapply("mult") -> FalsifiedResponse),
      "incByTwo" -> Map(Associativity -> VerifiedResponse, Commutativity -> VerifiedResponse),
      "min" -> Map(Associativity -> VerifiedResponse, Commutativity -> VerifiedResponse),
      "pow" -> Map(Associativity -> FalsifiedResponse, Commutativity -> FalsifiedResponse),
    ))(verifier.verify(module))
  }

  test("test doubleOperations module verification") {
    val module = compiledDoubleOperationsModule.typed
    val verifier = new Verifier()
    assertResult(Map(
      "add" -> Map(Associativity -> VerifiedResponse, Commutativity -> VerifiedResponse, HasUnapply("sub") -> VerifiedResponse),
      "sub" -> Map(Associativity -> FalsifiedResponse, Commutativity -> FalsifiedResponse, HasUnapply("add") -> VerifiedResponse),
      "mult" -> Map(Associativity -> VerifiedResponse, Commutativity -> VerifiedResponse, HasUnapply("div") -> FalsifiedResponse),
      "div" -> Map(Associativity -> FalsifiedResponse, Commutativity -> FalsifiedResponse, HasUnapply("mult") -> FalsifiedResponse),
      "min" -> Map(Associativity -> VerifiedResponse, Commutativity -> VerifiedResponse),
    ))(verifier.verify(module))
  }

  /*
    test("test nonZeroDoubles module verification") {
    val module = compiledNonZeroDoublesModule.typed
    val verifier = new Verifier()
    assertResult(Map(
      "add" -> Map(Associativity -> VerifiedResponse, Commutativity -> VerifiedResponse, HasUnapply("sub") -> VerifiedResponse),
      "sub" -> Map(Associativity -> FalsifiedResponse, Commutativity -> FalsifiedResponse, HasUnapply("add") -> VerifiedResponse),
      "mult" -> Map(Associativity -> VerifiedResponse, Commutativity -> VerifiedResponse, HasUnapply("div") -> VerifiedResponse),
      "div" -> Map(Associativity -> FalsifiedResponse, Commutativity -> FalsifiedResponse, HasUnapply("mult") -> VerifiedResponse),
      "min" -> Map(Associativity -> VerifiedResponse, Commutativity -> VerifiedResponse),
    ))(verifier.verify(module))
  }
  */

  test("test stringOperations module verification") {
    val module = compiledStringOperationsModule.typed
    val verifier = new Verifier()
    assertResult(Map(
      "concat" -> Map(Associativity -> VerifiedResponse, Commutativity -> FalsifiedResponse),
    ))(verifier.verify(module))
  }

  // joinInterval ist nicht assoziativ, solange man Intervalle erstellen kann mit l > h
  test("test interval module verification") {
    val module = compiledIntervalLattice.typed
    val verifier = new Verifier()
    assertResult(Map(
      "joinVal" -> Map(Associativity -> FalsifiedResponse, Commutativity -> VerifiedResponse),
      "joinBool" -> Map(Associativity -> VerifiedResponse, Commutativity -> VerifiedResponse),
      "joinInterval" -> Map(Associativity -> FalsifiedResponse, Commutativity -> VerifiedResponse)
    ))(verifier.verify(module))
  }

  test("test modified interval module verification") {
    val module = compiledModifiedIntervalLattice.typed
    val verifier = new Verifier()
    assertResult(Map(
      "joinVal" -> Map(Associativity -> VerifiedResponse, Commutativity -> VerifiedResponse),
      "joinBool" -> Map(Associativity -> VerifiedResponse, Commutativity -> VerifiedResponse),
      "joinInterval" -> Map(Associativity -> VerifiedResponse, Commutativity -> VerifiedResponse)
    ))(verifier.verify(module))
  }

  test("test interval module with invariants verification") {
    val module = compiledIntervalLatticeInvariants.typed
    val verifier = new Verifier()
    assertResult(Map(
      "joinVal" -> Map(Associativity -> VerifiedResponse, Commutativity -> VerifiedResponse),
      "joinBool" -> Map(Associativity -> VerifiedResponse, Commutativity -> VerifiedResponse),
      "joinInterval" -> Map(Associativity -> VerifiedResponse, Commutativity -> VerifiedResponse)
    ))(verifier.verify(module))
  }

  test("test bool lattice module verification") {
    val module = compiledBoolLattice.typed
    val verifier = new Verifier()
    assertResult(Map(
      "joinBool" -> Map(Associativity -> VerifiedResponse, Commutativity -> VerifiedResponse)
    ))(verifier.verify(module))
  }

  test("test intOpsSignLattice module verification") {
    val module = compiledIntOpsSignLattice.typed
    val verifier = new Verifier()
    assertResult(Map(
      "add" -> Map(Associativity -> VerifiedResponse, Commutativity -> VerifiedResponse, HasUnapply("sub") -> VerifiedResponse),
      "sub" -> Map(Associativity -> FalsifiedResponse, Commutativity -> FalsifiedResponse, HasUnapply("add") -> VerifiedResponse),
      "mult" -> Map(Associativity -> VerifiedResponse, Commutativity -> VerifiedResponse),
      "div" -> Map(Associativity -> FalsifiedResponse, Commutativity -> FalsifiedResponse),
      "addSign" -> Map(SoundnessAnno("add", "intToSign", "intToSign", "leqSign") -> VerifiedResponse,
        MonotonicityAnno("leqSign", "leqSign") -> VerifiedResponse),
      "subSign" -> Map(SoundnessAnno("sub", "intToSign", "intToSign", "leqSign") -> VerifiedResponse,
        MonotonicityAnno("leqSign", "leqSign") -> VerifiedResponse),
      "multSign" -> Map(SoundnessAnno("mult", "intToSign", "intToSign", "leqSign") -> VerifiedResponse,
        MonotonicityAnno("leqSign", "leqSign") -> VerifiedResponse),
      // divSign ist keine sound abstraction von div, da zB z3 0 / 1 zu 0 auswertet, aber wir in divSign
      // Zero / Pos zu Bot auswerten
      "divSign" -> Map(SoundnessAnno("div", "intToSign", "intToSign", "leqSign") -> FalsifiedResponse,
        MonotonicityAnno("leqSign", "leqSign") -> VerifiedResponse),
      "gtSign" -> Map(SoundnessAnno("gt", "intToSign", "booleanToBool", "leqBool") -> VerifiedResponse,
        MonotonicityAnno("leqSign", "leqBool") -> VerifiedResponse),
      "equalsSign" -> Map(SoundnessAnno("equals", "intToSign", "booleanToBool", "leqBool") -> VerifiedResponse,
        MonotonicityAnno("leqSign", "leqBool") -> VerifiedResponse),
      "leqSign" -> Map(PartialOrderAnno -> VerifiedResponse),
      "leqBool" -> Map(PartialOrderAnno -> VerifiedResponse)
    ))(verifier.verify(module))
  }
}
