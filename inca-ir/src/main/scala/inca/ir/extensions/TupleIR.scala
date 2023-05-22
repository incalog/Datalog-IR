package inca.ir.extensions

import inca.ir.{Term, Type, Atom, BaseIR, Language}

case class TTuple(tys: Seq[Type]) extends Type:
  override def toString: String = tys.mkString("(", ", ", ")")

case class Tuple(ts: Seq[Term]) extends Term:
  override def toString: String = ts.mkString("(", ", ", ")")

case class Project(t: Term, idx: Int) extends Term:
  override def toString: String = s"$t._$idx"

trait TupleIR extends BaseIR:
  override val name: String = "Tuple"
  override def language: Language = super.language + new TupleIR {}
  override def requires: Language = Language()
