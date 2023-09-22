package inca.ir.extension.arithmetic

import inca.ir.*
import inca.ir.extension.block
import inca.ir.extension.bool


// TODO Discuss: Do we want to support doubles in this IR or do we want to split the IR in Int and double IR ?

// TODO: With the design below we probably want a cast term as well. Something such as asInt, asDouble or we do it
//  implicitly.

case object TInt extends Type
case object TDouble extends Type

case class IntNum(value: Int) extends Term:
  override def toString: String = s"$value"

case class DoubleNum(value: Double) extends Term:
  override def toString: String = s"$value"

trait BinOp(lhs: Term, rhs: Term, op: String) extends Term:
  override def toString: String = s"$lhs $op $rhs"
  override def vars: Seq[Var] = lhs.vars ++ rhs.vars

case class Add(lhs: Term, rhs: Term) extends BinOp(lhs, rhs, "+")
case class Sub(lhs: Term, rhs: Term) extends BinOp(lhs, rhs, "-")
case class Mul(lhs: Term, rhs: Term) extends BinOp(lhs, rhs, "*")
case class Div(lhs: Term, rhs: Term) extends BinOp(lhs, rhs, "/")
case class Min(lhs: Term, rhs: Term) extends BinOp(lhs, rhs, "min")
case class Max(lhs: Term, rhs: Term) extends BinOp(lhs, rhs, "min")

case class LT(lhs: Term, rhs: Term) extends Atom:
  override def toString: String = s"$lhs < $rhs"
  override def vars: Seq[Var] = lhs.vars ++ rhs.vars

case class GT(lhs: Term, rhs: Term) extends Atom:
  override def toString: String = s"$lhs > $rhs"
  override def vars: Seq[Var] = lhs.vars ++ rhs.vars

object IR extends IR { }
trait IR extends BaseIR:
  override val name: String = "Arithmetic"
  override def language: Language = super.language + IR
  override def requires: Language = Language(IR)