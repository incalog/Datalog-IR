package inca.ir.extensions

import inca.ir.{BaseIR, Language, Term, Type}

// TODO Discuss: Do we want to include SetComprehension in the IR ?
//  We most certainly need type information to defunctionalize
//  How do we encode empty sets ?

case class TSet(ty: Type) extends Type:
  override def toString: String = s"Set[$ty]"

case class Set(ts: Seq[Term]) extends Term:
  override def toString: String = ts.mkString("Set(", ", ", ")")

case class SetUnion(t1: Term, t2: Term) extends Term:
  override def toString: String = t1.toString + " ∪ " + t2

case class SetIntersection(t1: Term, t2: Term) extends Term:
  override def toString: String = t1.toString + " ∩ " + t2

trait SetIR extends BaseIR:
  override val name: String = "Set"
  override def language: Language = super.language + new SetIR {}
  override def requires: Language = Language()