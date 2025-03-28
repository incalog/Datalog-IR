package inca.ir.extension.not

import inca.ir.*

trait IR extends BaseIR:
  override val name: String = "Not"
  override def language: Language = super.language + IR
  override def requires: Language = Language()

object IR extends IR {}

/**
 * There is a good chance you want to use WeakNot instead of Not. Check the command below about WeakNot before
 * you proceed to using Not.
 */
case class Not(at: Atom) extends Atom:
  override def toString: String = s"not($at)"
  override def vars: Seq[Var] = at.vars
  override def commonVars: Set[Var] = at.commonVars

/**
 * WeakNot is a less strict-form of not. If a variable was binding in an atom `a`, and we negate this atom
 * then `Not(a)` requires the variable to be bound. However, that is not the desired behaviour in all cases.
 *
 * E.g.
 *  R(x) :- x == 5.
 *  Q(y) :- Not(R(x)), y == 1.
 *
 * Here, Not(R(x)) is an existential query that expresses that no value at all exists in R.
 * This is equivalent to writing the negated call ~R(x) or ~R(_). However, Not(R(x)) checks R(x) with a negative
 * polarity, since R(x) had positive polarity. That is, all variables in the call to R must be bound.
 * In particular, x must now be bound and as such the code above does not type check.
 * This is where WeakNot comes in. WeakNot checks R(x) under a neutral, collapsing polarity. That is,
 * x can still be binding.
 */
case class WeakNot(at: Atom) extends Atom:
  override def toString: String = s"weaknot($at)"
  override def vars: Seq[Var] = at.vars
  override def commonVars: Set[Var] = at.commonVars