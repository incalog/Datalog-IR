package inca.ir.extension.locals

import inca.ir.*

/**
 * This IR extensions makes assignments in Datalog explicit.
 * This way, this extension abstracts away the need for SSA transforming higher-level languages that support local
 * mutation, such as OODL.
 */

trait IR extends BaseIR:
  override val name: String = "Locals"
  override def language: Language = super.language + IR
  override def requires: Language = Language()

object IR extends IR {}

// Reassign variable
case class Assign(v: Var, t: Term) extends Atom:
  override def vars: Seq[Var] = v +: t.vars
  override def commonVars: Set[Var] = v.commonVars ++ t.commonVars