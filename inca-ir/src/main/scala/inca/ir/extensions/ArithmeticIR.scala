package inca.ir.extensions

import inca.ir.*


// TODO Discuss: Do we want to support doubles ? or do we want to split the IR in Int and double IR or
//  do we want to make the IR generic in the number type ?

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

case class LT(lhs: Term, rhs: Term) extends Term:
  override def toString: String = s"$lhs < $rhs"

case class GT(lhs: Term, rhs: Term) extends Term:
  override def toString: String = s"$lhs > $rhs"

trait ArithmeticIR extends BaseIR:
  override val name: String = "Arithmetic"
  override def language: Language = super.language + new ArithmeticIR {}
  override def requires: Language = Language()