package inca.ir.extension.bool

import inca.ir.*
import inca.ir.extension.*

trait IR extends BaseIR:
  override val name: String = "Boolean"
  override def language: Language = super.language + IR
  override def requires: Language = Language(arithmetic.IR, block.IR, disjunction.IR, not.IR)
object IR extends IR { }

case object TBoolean extends Type

sealed trait BoolTerm extends Term

case class AtomAsBool(a: Atom) extends BoolTerm:
  override def vars: Seq[Var] = a.vars

case class BoolAnd(t1: Term, t2: Term) extends BoolTerm:
  override def toString: String = s"$t1 && $t2"
  override def vars: Seq[Var] = t1.vars ++ t2.vars

case class BoolOr(t1: Term, t2: Term) extends BoolTerm:
  override def toString: String = s"$t1 || $t2"
  override def vars: Seq[Var] = t1.vars ++ t2.vars

case class BoolNot(t: Term) extends BoolTerm:
  override def toString: String = s"!$t"
  override def vars: Seq[Var] = t.vars

case object BoolTrue extends BoolTerm:
  override def vars: Seq[Var] = Seq()
  override def toString: String = "true"

case object BoolFalse extends BoolTerm:
  override def vars: Seq[Var] = Seq()
  override def toString: String = "false"
