package inca.ir.extension.bool.printer

import inca.ir.extension.bool.{TBoolean, BoolTrue, BoolFalse, BoolOr, BoolAnd, BoolNot, BoolTerm, AtomAsBool}
import inca.ir.{Arg, Atom, Term, Type}
import inca.ir.printer.BaseIRPrinter

trait Printer extends BaseIRPrinter:
  override def prettyPrint(ty: Type): String = ty match
    case TBoolean => "TBoolean"
    case _ => super.prettyPrint(ty)

  override def prettyPrint(term: Term): String = term match
    case AtomAsBool(a) => s"AtomAsBool(${prettyPrint(a)})"
    case BoolAnd(t1, t2) => s"(${prettyPrint(t1)} && ${prettyPrint(t2)})"
    case BoolOr(t1, t2) => s"(${prettyPrint(t1)} || ${prettyPrint(t2)})"
    case BoolNot(t) => s"!${prettyPrint(t)}"
    case BoolTrue => "true"
    case BoolFalse => "false"
    case _ => super.prettyPrint(term)
