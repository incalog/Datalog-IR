package inca.ir.extension.block

import inca.ir.*

/** Introduces a block {atoms, t} that allows atoms embedded in terms t.
 * The lowering will lift these atoms to the surrounding rule body.
 *
 * Important: No disjunctions may exist when blocks are being lowered, since the
 * lowering assumes conjunctive atoms.
 */
trait IR extends BaseIR:
  override val name: String = "Block"
  override def language: Language = super.language + IR
  override def requires: Language = Language()

object IR extends IR {}

case class Block(at: Seq[Atom], t: Term) extends Term:
  override def toString: String =
    if (at.isEmpty)
      t.toString
    else
      s"{${at.mkString(", ")}; $t}"

  override def vars: Seq[Var] = t.vars ++ at.flatMap(_.vars)
  override def commonVars: Set[Var] = at.flatMap(_.commonVars).toSet ++ t.commonVars

object Block:
  def apply(at: Atom, t: Term): Block = Block(Seq(at), t)