package inca.ir.extension.locals

import inca.ir.*

/**
 * This IR extensions makes assignments in Datalog explicit. That way, we can declare mutable and immutable variables
 * in Datalog. Equality constraint will ONLY behave as comparisons if this language feature is used.
 * This way, this extension abstracts away the need for SSA transforming higher-level languages that support local
 * mutation, such as OODL.
 */

trait IR extends BaseIR:
  override val name: String = "Locals"
  override def language: Language = super.language + IR
  override def requires: Language = Language()

object IR extends IR {}

// Mutable variable
case class DeclVar(v: Var, t: Term) extends Atom:
  override def vars: Seq[Var] = v +: t.vars
  override def commonVars: Set[Var] = v.commonVars ++ t.commonVars

// Immutable variable
case class DeclVal(v: Var, t: Term) extends Atom:
  override def vars: Seq[Var] = v +: t.vars
  override def commonVars: Set[Var] = v.commonVars ++ t.commonVars

// Reassign mutable variable
case class Assign(v: Var, t: Term) extends Atom:
  override def vars: Seq[Var] = v +: t.vars
  override def commonVars: Set[Var] = v.commonVars ++ t.commonVars