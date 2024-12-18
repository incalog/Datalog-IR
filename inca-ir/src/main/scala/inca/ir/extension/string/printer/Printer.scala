package inca.ir.extension.string.printer

import inca.ir.{Term, Type}
import inca.ir.extension.string.{TString, StringLit, StringConcat, ToString}
import inca.ir.printer.BaseIRPrinter

trait Printer extends BaseIRPrinter:
  override def prettyPrint(term: Term): String = term match
    case StringLit(value) => s"\"$value\""
    case StringConcat(lhs, rhs) => s"${prettyPrint(lhs)} + ${prettyPrint(lhs)}"
    case ToString(t) => s"${prettyPrint(t)}.toString"
    case _ => super.prettyPrint(term)

  override def prettyPrint(ty: Type): String = ty match
    case TString => "TString"
    case _ => super.prettyPrint(ty)