package inca.ir.extension.arithmetic

import inca.ir.*
import inca.ir.extension.aggregate.AggregationOperatorBuiltIn
import inca.ir.extension.block
import inca.ir.extension.bool

case object TInt extends Type
case object TDouble extends Type

case class IntNum(value: Int) extends Term:
  override def vars: Seq[Var] = Seq()
  override def toString: String = s"$value"

case class DoubleNum(value: Double) extends Term:
  override def vars: Seq[Var] = Seq()
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
def Max(lhs: Term, rhs: Term): BinOp = BinOp(lhs, rhs, "max")
def Abs(t: Term): UnOp = UnOp(t, "abs")

def LT(lhs: Term, rhs: Term): BinCompare = BinCompare(lhs, rhs, "<")
def LE(lhs: Term, rhs: Term): BinCompare = BinCompare(lhs, rhs, "<=")
def GT(lhs: Term, rhs: Term): BinCompare = BinCompare(lhs, rhs, ">")
def GE(lhs: Term, rhs: Term): BinCompare = BinCompare(lhs, rhs, ">=")

enum ArithmeticAggregationOperator extends AggregationOperatorBuiltIn:
  case Count
  case SumInt
  case MinInt
  case MaxInt
  case SumDouble
  case MinDouble
  case MaxDouble

  override def resultType: Type = this match
    case Count | SumInt | MinInt | MaxInt => TInt
    case SumDouble | MinDouble | MaxDouble => TDouble

  override def typecheck(in: Seq[Type]): Option[String] = this match
    case ArithmeticAggregationOperator.Count =>
      if (in.size == 1)
        None
      else
        Some(s"Cannot compute $this for values of type $in")
    case SumInt | MinInt | MaxInt =>
      if (in == Seq(TInt))
        None
      else
        Some(s"Cannot compute $this for values of type $in")
    case SumDouble | MinDouble | MaxDouble =>
      if (in == Seq(TDouble))
        None
      else
        Some(s"Cannot compute $this for values of type $in")

object IR extends IR { }
trait IR extends BaseIR:
  override val name: String = "Arithmetic"
  override def language: Language = super.language + IR
  override def requires: Language = Language(IR)