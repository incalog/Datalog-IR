package inca.frontend.souffle.compiler

import inca.frontend.souffle.Syntax._
import inca.frontend.souffle.{Parser, PrettyPrinter}
import org.scalatest.funsuite.AnyFunSuite

class EliminateRuleDisjunctionTest extends AnyFunSuite {

  test("rule") {
    val rule = Parser.parse(Parser.rule, "A(x), B(x) :- C(x); (D(x), (E(x); F(x))).")

    val r = EliminateRuleDisjunction.eliminateRuleDisjunction(rule)
    r.foreach(PrettyPrinter.print)
  }
}
