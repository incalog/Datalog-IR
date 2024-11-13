package inca.ir.extension.not

import inca.ir.*

/** Introduces a block {atoms, t} that allows atoms embedded in terms t.
 * The lowering will lift these atoms to the surrounding rule body.
 *
 * Important: No disjunctions may exist when blocks are being lowered, since the
 * lowering assumes conjunctive atoms.
 */
trait IR extends BaseIR:
  override val name: String = "Not"

  override def language: Language = super.language + IR

  override def requires: Language = Language()

object IR extends IR {}

case class Not(at: Atom) extends Atom:
  override def toString: String = s"not($at)"

  override def vars: Seq[Var] = at.vars

case class WeakNot(at: Atom) extends Atom:
  override def toString: String = s"weaknot($at)"

  override def vars: Seq[Var] = at.vars