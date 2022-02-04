package inca.frontend.functional.verification

import inca.examples.functional.ControlDataFlow
import inca.frontend.functional.core.FunctionDef
import inca.frontend.functional.parser.Parser
import org.scalatest.funsuite.AnyFunSuite
import smtlib.trees.Commands.PropLiteral
import smtlib.trees.Terms.SSymbol
import inca.frontend.functional.verification.Verifier
import inca.frontend.functional.verification.examples.Lattices.{const_lattice_module, sign_lattice_module}

class GenerateSMTLIBTest extends AnyFunSuite {

  test("generate data type") {
    print(PropLiteral(SSymbol("x"), true))
    assertResult(1)(1)
  }
  test("what does the functional AS look like") {
    print(Parser.parse(ControlDataFlow.IntervalModule))
  }
  test("print sign lattice") {
    print(sign_lattice_module)
  }
  test("print constant Propagation lattice") {
    print(const_lattice_module)
  }
  test("collect called functions") {
    val module = Parser.parse(ControlDataFlow.IntervalModule)
    val verifier = new Verifier()
    print(verifier.collectCalledFunctions(module.content(2).asInstanceOf[FunctionDef].body))
    print(verifier.collectCalledFunctions(module.content(4).asInstanceOf[FunctionDef].body))
  }
  test("collect used datatypes") {
    val module = Parser.parse(ControlDataFlow.IntervalModule)
    val verifier = new Verifier()
    verifier.fillDicts(module)
    print(verifier.collectUsedDataDefs(module.content(2).asInstanceOf[FunctionDef]))
    print(verifier.collectUsedDataDefs(module.content(4).asInstanceOf[FunctionDef]))
  }
}
