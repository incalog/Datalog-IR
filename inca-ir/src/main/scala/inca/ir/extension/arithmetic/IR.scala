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

case class BinOp(lhs: Term, rhs: Term, op: String) extends Term:
  override def toString: String =
    if (analysis.isEmpty)
      s"$lhs $op $rhs"
    else
      s"($lhs $op $rhs)" + analysisString
  override def vars: Seq[Var] = lhs.vars ++ rhs.vars

case class UnOp(t: Term, op: String) extends Term:
  override def toString: String = s"$op $t"
  override def vars: Seq[Var] = t.vars

case class BinCompare(lhs: Term, rhs: Term, op: String) extends Atom:
  override def toString: String = s"$lhs $op $rhs" + analysisString
  override def vars: Seq[Var] = lhs.vars ++ rhs.vars

def Add(lhs: Term, rhs: Term): BinOp = BinOp(lhs, rhs, "+")
def Sub(lhs: Term, rhs: Term): BinOp = BinOp(lhs, rhs, "-")
def Mul(lhs: Term, rhs: Term): BinOp = BinOp(lhs, rhs, "*")
def Div(lhs: Term, rhs: Term): BinOp = BinOp(lhs, rhs, "/")
def Remainder(lhs: Term, rhs: Term): BinOp = BinOp(lhs, rhs, "%")
def Min(lhs: Term, rhs: Term): BinOp = BinOp(lhs, rhs, "min")
def Max(lhs: Term, rhs: Term): BinOp = BinOp(lhs, rhs, "min")
def Abs(t: Term): UnOp = UnOp(t, "abs")

def LT(lhs: Term, rhs: Term): BinCompare = BinCompare(lhs, rhs, "<")
def LE(lhs: Term, rhs: Term): BinCompare = BinCompare(lhs, rhs, "<=")
def GT(lhs: Term, rhs: Term): BinCompare = BinCompare(lhs, rhs, ">")
def GE(lhs: Term, rhs: Term): BinCompare = BinCompare(lhs, rhs, ">=")

object IR extends IR { }
trait IR extends BaseIR:
  override val name: String = "Arithmetic"
  override def language: Language = super.language + IR
  override def requires: Language = Language(IR)