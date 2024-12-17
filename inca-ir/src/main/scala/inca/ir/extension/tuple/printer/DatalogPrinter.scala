package inca.ir.extension.tuple.printer

import inca.ir.{Term, Type}
import inca.ir.extension.tuple.{TTuple, TupleLit, Project}
import inca.ir.printer.DatalogBaseIRPrinter

trait DatalogPrinter extends DatalogBaseIRPrinter:
  override def prettyPrint(term: Term): String = term match
    case TupleLit(ts) => ts.map(prettyPrint).mkString("(", ", ", ")")
    case Project(t, idx) => s"${prettyPrint(t)}._${idx + 1}"
    case _ => super.prettyPrint(term)

  override def prettyPrint(ty: Type): String = ty match
    case TTuple(tys) => tys.map(prettyPrint).mkString("(", ", ", ")")
    case _ => super.prettyPrint(ty)


