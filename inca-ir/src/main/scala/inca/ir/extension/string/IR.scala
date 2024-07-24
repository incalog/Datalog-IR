package inca.ir.extension.string

import inca.ir.*
import inca.ir.extension.block
import inca.ir.extension.bool


case object TString extends Type

case class StringLit(value: String) extends Term:
  override def vars: Seq[Var] = Seq()
  override def toString: String = s"\"$value\""

case class StringConcat(lhs: Term, rhs: Term) extends Term:
  override def toString: String = s"$lhs + $rhs"
  override def vars: Seq[Var] = lhs.vars ++ rhs.vars

case class ToString(t: Term) extends Term:
  override def toString: String = s"$t.toString"
  override def vars: Seq[Var] = t.vars

case class Length(t: Term) extends Term:
    override def toString: String = s"$t.length"
    override def vars: Seq[Var] = t.vars

case class SubstringFrom(t: Term, index: Term) extends Term:
  override def toString: String = s"$t.substring($index)"
  override def vars: Seq[Var] = t.vars

case class SubstringFromTo(t: Term, start: Term, end: Term) extends Term:
  override def toString: String = s"$t.substring($start, $end)"
  override def vars: Seq[Var] = t.vars

case class LastIndexOf(t: Term, sub: Term) extends Term:
  override def toString: String = s"$t.lastIndexOf($sub)"
  override def vars: Seq[Var] = t.vars

object IR extends IR { }
trait IR extends BaseIR:
  override val name: String = "String"
  override def language: Language = super.language + IR
  override def requires: Language = Language(IR)