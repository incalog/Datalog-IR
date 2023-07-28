package inca.ir.extension.set

import inca.ir.*

// TODO Discuss: Do we want to include SetComprehension in the IR ?
//  We need type information to defunctionalize
//  How do we encode empty sets ?

case class TSet(ty: Type) extends Type:
  override def toString: String = s"Set[$ty]"

case class Set(ts: Seq[Term]) extends Term:
  override def toString: String = ts.mkString("Set(", ", ", ")")
  override def vars: Seq[Var] = ts.flatMap(_.vars)
object Set:
  def from(ts: Term*): Set = new Set(ts)

case class SetUnion(t1: Term, t2: Term) extends Term:
  override def toString: String = t1.toString + " ∪ " + t2
  override def vars: Seq[Var] = t1.vars ++ t2.vars

case class SetIntersection(t1: Term, t2: Term) extends Term:
  override def toString: String = t1.toString + " ∩ " + t2
  override def vars: Seq[Var] = t1.vars ++ t2.vars

case class SetMember(t1: Term, t2: Term) extends Atom

trait IR extends BaseIR:
  override val name: String = "Set"
  override def language: Language = super.language + new IR {}
  override def requires: Language = Language()
object IR extends IR {}