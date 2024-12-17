package inca.ir.extension.list

import inca.ir.*

trait IR extends BaseIR:
  override val name: String = "List"
  override def language: Language = super.language + IR
  override def requires: Language = Language()

case class TList(ty: Type) extends Type

object IR extends IR {}

case class ListLit(ts: Seq[Term]) extends Term:
  override def vars: Seq[Var] = ts.flatMap(_.vars)

object ListLit:
  def from(ts: Term*): ListLit = new ListLit(ts)
  def empty: ListLit = new ListLit(Seq())

case class Size(list: Term) extends Term:
  override def vars: Seq[Var] = list.vars

case class Head(list: Term) extends Term:
  override def vars: Seq[Var] = list.vars

case class Tail(list: Term) extends Term:
  override def vars: Seq[Var] = list.vars

case class IsEmpty(list: Term) extends Term:
  override def vars: Seq[Var] = list.vars

case class Append(list: Term, element: Term) extends Term:
  override def vars: Seq[Var] = list.vars ++ element.vars

case class Prepend(list: Term, element: Term) extends Term:
  override def vars: Seq[Var] = element.vars ++ list.vars

case class Deconstruct(list: Term, hd: Term, tail: Term) extends Atom:
  override def vars: Seq[Var] = list.vars ++ hd.vars ++ tail.vars
