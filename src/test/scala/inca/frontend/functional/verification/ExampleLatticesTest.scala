package inca.frontend.functional.verification

import inca.frontend.functional.core
import inca.frontend.functional.core.{Associativity, Commutativity}
import org.scalatest.funsuite.AnyFunSuite
import inca.frontend.functional.verification.examples.Lattices.{const_lattice_module, signVal_lattice_module, sign_lattice, sign_lattice_module}
import inca.util.Gensym
import smtlib.Interpreter
import smtlib.interpreters.Z3Interpreter

class ExampleLatticesTest extends AnyFunSuite {
  test("test sign_lattice_module verification") {
    val module = sign_lattice_module
    val verifier = new Verifier()
    assertResult(Map("join" -> Map("assoc" -> "true", "comm" -> "true")))(verifier.verify(module))
  }

  test("test const_lattice_module verification") {
    val module = const_lattice_module
    val verifier = new Verifier()
    assertResult(Map("join" -> Map("assoc" -> "true", "comm" -> "true")))(verifier.verify(module))
  }

  test("test signVal_lattice_module verification") {
    val module = signVal_lattice_module
    val verifier = new Verifier()
    assertResult(Map("join" -> Map("assoc" -> "true", "comm" -> "true"),
      "joinBool" -> Map("assoc" -> "true", "comm" -> "true"),
      "joinSign" -> Map("assoc" -> "true", "comm" -> "true")))(verifier.verify(module))
  }
}
