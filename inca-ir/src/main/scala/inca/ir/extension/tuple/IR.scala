package inca.ir.extension.tuple

import inca.ir.*

object IR extends IR { }
trait IR extends BaseIR:
  override val name: String = "Tuple"
  override def language: Language = super.language + IR
  override def requires: Language = Language()

case class TTuple(tys: Seq[Type]) extends Type:
  override def toString: String = tys.mkString("(", ", ", ")")
  override def size: Int = tys.map(_.size).sum
  override def flatten: Seq[Type] = tys.flatMap(_.flatten)

case class Tuple(ts: Seq[Term]) extends Term:
  override def toString: String = ts.mkString("(", ", ", ")")
  override def vars: Seq[Var] = ts.flatMap(_.vars)

case class Project(t: Term, idx: Int) extends Term:
  override def toString: String = s"$t._$idx"
  override def vars: Seq[Var] = t.vars

