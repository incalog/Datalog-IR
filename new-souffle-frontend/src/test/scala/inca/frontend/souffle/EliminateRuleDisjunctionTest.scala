package inca.frontend.souffle

import inca.frontend.souffle.Syntax.{Atom, Conjunction, ConjunctionTerm, ConjunctionTermAtom, ConjunctionTermDisjunction, Disjunction, QualifiedName}
import org.scalatest.funsuite.AnyFunSuite

class EliminateRuleDisjunctionTest extends AnyFunSuite {

  def makeAtom(id: String): Atom = Atom(QualifiedName(id.split('.')), Seq())
  def makeTermAtom(id: String): ConjunctionTermAtom =
    ConjunctionTermAtom(isNegated = false, Atom(QualifiedName(id.split('.')), Seq()))
  def makeTermDisjunction(conjunctions: Seq[Conjunction]): ConjunctionTermDisjunction =
    ConjunctionTermDisjunction(isNegated = false, Disjunction(conjunctions))

  test("crossProduct") {
    val as = Seq(Conjunction(Seq(makeTermAtom("A"))))
    val bs = Seq(
      Conjunction(Seq(makeTermAtom("B"))),
      Conjunction(Seq(makeTermAtom("C"))),
    )

    val r = EliminateRuleDisjunction.crossProduct(as, bs)

    r.foreach(r => println(PrettyPrinter.stringify(r)))
  }

  test("resolveNegatedDisjunctions") {
//    var t = ConjunctionTermDisjunction(true, Disjunction(Seq(
//      Conjunction(),
//      Conjunction(),
//    )))
//
//    EliminateRuleDisjunction.resolveNegatedDisjunctions(t)
  }

  test("extractConjunctions1") {
    val disjunction = Parser.parse(Parser.disjunction, "C(); (D(), (E(); F(), G(), (H(); I())))")
    val conjunctions = disjunction.conjunctions.map(EliminateRuleDisjunction.extractConjunctions)
    println(conjunctions)

    conjunctions.foreach(_.foreach(PrettyPrinter.print))
  }

  test("extractConjunctions2") {

    // A, (B; C), (D; E, F)
    // =>
    // A, B, D
    // A, B, E, F
    // A, C, D
    // A, C, E, F

    val c = Conjunction(Seq(
      makeTermAtom("A"),
      makeTermDisjunction(Seq(
        Conjunction(Seq(makeTermAtom("B"))),
        Conjunction(Seq(makeTermAtom("C"))),
      )),
      makeTermDisjunction(Seq(
        Conjunction(Seq(makeTermAtom("D"))),
        Conjunction(Seq(
          makeTermAtom("E"),
          makeTermAtom("F"),
        )),
      )),
    ))

    val r = EliminateRuleDisjunction.extractConjunctions(c)

    r.foreach(PrettyPrinter.print)
  }

  test("rule") {
    val rule = Parser.parse(Parser.rule, "A(x), B(x) :- C(x); (D(x), (E(x); F(x))).")

    val r = EliminateRuleDisjunction.eliminateRuleDisjunction(rule)
    r.foreach(PrettyPrinter.print)
  }
}
