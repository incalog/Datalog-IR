package inca.ir.extension.arithmetic.printer

import inca.ir.extension.arithmetic.{UnOp, BinCompare, BinOp, DoubleNum, IntNum, TDouble, TInt}
import inca.ir.{Arg, Atom, Term, Type}
import inca.ir.printer.DatalogBaseIRPrinter

trait DatalogPrinter extends DatalogBaseIRPrinter:
  override def prettyPrint(atom: Atom): String = atom match
    case BinCompare(lhs, rhs, op) => s"(${prettyPrint(lhs)} $op ${prettyPrint(rhs)})"
    case _ => super.prettyPrint(atom)

  override def prettyPrint(ty: Type): String = ty match
    case TInt => "TInt"
    case TDouble => "TDouble"
    case _ => super.prettyPrint(ty)

  override def prettyPrint(term: Term): String = term match
    case IntNum(value) => value.toString
    case DoubleNum(value) => value.toString
    case UnOp(t, op) => s"$op ${prettyPrint(t)}"
    case BinOp(lhs, rhs, op) => s"(${prettyPrint(lhs)} $op ${prettyPrint(rhs)})"
    case _ => super.prettyPrint(term)
