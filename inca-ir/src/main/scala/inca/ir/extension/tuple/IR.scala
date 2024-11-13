package inca.ir.extension.tuple

import inca.ir.*

object IR extends IR {}

trait IR extends BaseIR:
  override val name: String = "Tuple"

  override def language: Language = super.language + IR

  override def requires: Language = Language()

case class TTuple(tys: Seq[Type]) extends Type:
  override def toString: String = tys.mkString("(", ", ", ")")

  override def size: Int = tys.map(_.size).sum

  override def flatten: Seq[Type] = tys.flatMap(_.flatten)

object TTuple:
  def make(ts: Seq[Type]): Type =
    if (ts.size == 1)
      ts.head
    else
      TTuple(ts)


case class TupleLit(ts: Seq[Term]) extends Term:
  if (ts.size == 1)
    throw new IllegalArgumentException(s"Unary tuples are not allowed.")

  override def toString: String = ts.mkString("(", ", ", ")")

  override def vars: Seq[Var] = ts.flatMap(_.vars)

object TupleLit:
  def make(t: Term, ts: Term*): Term =
    if (ts.isEmpty)
      t
    else
      new TupleLit(t +: ts)

  def make(ts: Seq[Term]): Term =
    if (ts.size == 1)
      ts.head
    else
      TupleLit(ts)

case class Project(t: Term, idx: Int) extends Term:
  override def toString: String = s"$t._${idx + 1}"

  override def vars: Seq[Var] = t.vars

