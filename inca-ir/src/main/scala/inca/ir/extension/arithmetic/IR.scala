package inca.ir.extension.arithmetic

import inca.ir.*
import inca.ir.extensions.PrimitiveScalaIR


// TODO Discuss: Do we want to support doubles din this IR or do we want to split the IR in Int and double IR ?

// TODO: With the design below we probably want a cast term as well. Something such as asInt, asDouble or we do it
//  implicitly.

case object TInt extends Type
case object TDouble extends Type

case class IntNum(value: Int) extends Term:
  override def toString: String = s"$value"

case class DoubleNum(value: Double) extends Term:
  override def toString: String = s"$value"

case class Add(lhs: Term, rhs: Term) extends Term:
  override def toString: String = s"$lhs + $rhs"

case class Mul(lhs: Term, rhs: Term) extends Term:
  override def toString: String = s"$lhs * $rhs"

case class Div(lhs: Term, rhs: Term) extends Term:
  override def toString: String = s"$lhs * $rhs"

case class Sub(lhs: Term, rhs: Term) extends Term:
  override def toString: String = s"$lhs - $rhs"

case class Min(lhs: Term, rhs: Term) extends Term:
  override def toString: String = s"min($lhs, $rhs)"

case class Max(lhs: Term, rhs: Term) extends Term:
  override def toString: String = s"max($lhs, $rhs)"

case class LT(lhs: Term, rhs: Term) extends Atom:
  override def toString: String = s"$lhs < $rhs"

case class GT(lhs: Term, rhs: Term) extends Atom:
  override def toString: String = s"$lhs > $rhs"

object IR extends IR { }
trait IR extends BaseIR:
  override val name: String = "Arithmetic"
  override def language: Language = super.language + IR
  override def requires: Language = Language(new PrimitiveScalaIR {})