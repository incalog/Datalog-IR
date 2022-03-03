package inca.frontend.souffle

import Syntax._

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

  def resolveNegatedDisjunctions(e: ConjunctionTerm): Conjunction = e match {
    case ConjunctionTermDisjunction(true, disjunction) =>
      // !(A; B) => !A, !B
      // !(A; B, C) => !A, !(B, C)
      // !(A; (B; C)) => !A, !(B; C)
      val terms = disjunction.conjunctions.map { conjunction =>
        if (conjunction.terms.length == 1) conjunction.terms.head
        else ConjunctionTermDisjunction(isNegated = true, Disjunction(Seq(conjunction)))
      }

      Conjunction(terms)
    case _ => Conjunction(Seq(e))
  }

  def crossProduct(as: Seq[Conjunction], bs: Seq[Conjunction]): Seq[Conjunction] = {
    // [A, B], [C, D] => [A, C], [A, D], [B, C], [B, D]
    for (a <- as; b <- bs)
      yield a ++ b
  }

  def extractConjunctions(e: Conjunction): Seq[Conjunction] = {

    /*
    *  Strategy
    *
    * - for each term in the conjunction extract their conjunctions into a list
    * - example: A, (B; C) => [[A], [B, C]]
    * - then, reduce the resulting list of lists by applying the cross product
    * - example: [[A], [B, C]] => [AB, AC]
    *
    * */

    val r = e.terms.map {
      case term@ConjunctionTermAtom(isNegated, atom) => Seq(Conjunction(Seq(term)))
      case term@ConjunctionTermConstraint(isNegated, constraint) => Seq(Conjunction(Seq(term)))
      case ConjunctionTermDisjunction(isNegated, disjunction) =>
        disjunction.conjunctions.map(extractConjunctions).reduce(_ ++ _)
    }

    r.reduce(crossProduct)
  }

  sealed case class Rule(head: Atom, conjunction: Conjunction, queryPlan: Option[QueryPlan])

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
    val conjunctions =
      e.disjunction.conjunctions
        .flatMap(extractConjunctions)

    atoms.flatMap(atom =>
      conjunctions.map(conjunction =>
        Rule(atom, conjunction, e.queryPlan)))
  }
}
