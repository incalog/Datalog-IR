package inca.frontend.souffle.compiler

import inca.frontend.souffle.Syntax
import inca.frontend.souffle.Syntax._

object EliminateRuleDisjunction {

  /*    Strategy
  *
  * A :- B; C
  * =>
  * A :- B
  * A :- C
  *
  * A :- B, (C; D)
  * =>
  * A :- B, C
  * A :- B, D
  *
  * B, (C; D)
  * => extract conjunctions [[B], [C, D]]
  * => fold to [BC, BD]
  *
  * */

  def crossProduct(as: Seq[TermConjunction], bs: Seq[TermConjunction]): Seq[TermConjunction] = {
    // [A, B], [C, D] => [A, C], [A, D], [B, C], [B, D]
    for (a <- as; b <- bs)
      yield TermConjunction(a.terms ++ b.terms)
  }

  def transformToDNF(e: TermConjunction): Seq[TermConjunction] = {
    // 1) push negation inwards (negation normal form, nnf)
    def negated(t: Term): Term = t match {
      case TermConjunction(terms, isNegated) => TermConjunction(terms, isNegated = !isNegated)
      case TermDisjunction(terms, isNegated) => TermDisjunction(terms, isNegated = !isNegated)
      case TermAtom(atom, isNegated) => TermAtom(atom, isNegated = !isNegated)
      case TermConstraint(constraint, isNegated) => TermConstraint(constraint, isNegated = !isNegated)
    }

    def pushNegation(t: Term): Term = t match {
      case term@TermAtom(_, _) => term
      case term@TermConstraint(_, _) => term
      case TermConjunction(terms, isNegated) =>
        if (isNegated) TermDisjunction(terms.map(t => pushNegation(negated(t))))
        else TermConjunction(terms.map(pushNegation))
      case TermDisjunction(terms, isNegated) =>
        if (isNegated) TermConjunction(terms.map(t => pushNegation(negated(t))))
        else TermDisjunction(terms.map(pushNegation))
    }

    val nnf = pushNegation(e)

    // 2) reduce nnf to dnf
    def reduceToDNF(t: Term): Seq[TermConjunction] = t match {
      case TermDisjunction(terms, isNegated) =>
        assert(!isNegated)
        terms.map(reduceToDNF).reduce(_ ++ _)
      case TermConjunction(terms, isNegated) =>
        assert(!isNegated)
        terms.map(reduceToDNF).reduce(crossProduct)
      case t@TermAtom(_, _) => Seq(TermConjunction(Seq(t)))
      case t@TermConstraint(_, _) => Seq(TermConjunction(Seq(t)))
    }

    // dnf
    reduceToDNF(nnf)
  }

  sealed case class Rule(head: Atom, conjunction: TermConjunction, queryPlan: Option[QueryPlan])

  def eliminateRuleDisjunction(e: Syntax.Rule): Seq[Rule] = {

    /*
    *   Strategies
    *
    * - desugar conjunction of disjunctions into disjunction of conjunctions
    * example:
    *    A, (B; C) ==> (A, B; A, C)
    *
    * - reduce head count to 1
    * example:
    *    A, B :- C
    *
    * => A :- C
    *    B :- C
    *
    * - avoid negations if possible
    * example:
    *   !(A; B) => !A, !B
    *
    * */

    val atoms = e.atoms
    val conjunctions: Seq[TermConjunction] =
      e.disjunction.terms.map {
        case t@TermConjunction(_, _) => t
        case t => TermConjunction(Seq(t))
      }.flatMap(transformToDNF)

    atoms.flatMap(atom =>
      conjunctions.map(conjunction =>
        Rule(atom, conjunction, e.queryPlan)))
  }
}
