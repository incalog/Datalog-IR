package inca.ir.extension.bool

import inca.ir.*
import inca.ir.extension.*
import inca.ir.extension.disjunction.IR
import inca.ir.extensions.ArithmeticIR

trait IR extends BaseIR:
  override val name: String = "Boolean"
  override def language: Language = super.language + IR
  override def requires: Language = Language(ArithmeticIR, block.IR, IR, not.IR)
object IR extends IR { }

case object TBoolean extends Type

case class BoolAtom(t: Term) extends Atom:
  override def toString: String = s"$t"

sealed trait BoolTerm extends Term

case class AtomAsBool(a: Atom) extends BoolTerm:
  override def toString: String = s"bool($a)"

case class BoolAnd(t1: Term, t2: Term) extends BoolTerm:
  override def toString: String = s"$t1 && $t2"

case class BoolOr(t1: Term, t2: Term) extends BoolTerm:
  override def toString: String = s"$t1 || $t2"

case class BoolNot(t: Term) extends BoolTerm:
  override def toString: String = s"!$t"

case object BoolTrue extends BoolTerm:
  override def toString: String = "true"

case object BoolFalse extends BoolTerm:
  override def toString: String = "false"
