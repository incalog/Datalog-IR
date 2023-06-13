package inca.ir.extensions

import inca.ir.*

case object TBoolean extends Type

case class BoolAtom(t: Term) extends Atom:
  override def toString: String = s"$t"

case class BoolAnd(t1: Term, t2: Term) extends Term:
  override def toString: String = s"$t1 && $t2"

case class BoolOr(t1: Term, t2: Term) extends Term:
  override def toString: String = s"$t1 || $t2"

case class BoolNot(t: Term) extends Term:
  override def toString: String = s"!$t"

case object BoolTrue extends Term:
  override def toString: String = "true"

case object BoolFalse extends Term:
  override def toString: String = "false"

trait BooleanIR extends BaseIR:
  override val name: String = "Boolean"
  override def language: Language = super.language + new BooleanIR {}
  override def requires: Language = Language(new PrimitiveScalaIR {})