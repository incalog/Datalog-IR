package inca.ir.extension.block.printer

import inca.ir.extension.block.Block
import inca.ir.Term
import inca.ir.printer.DatalogBaseIRPrinter

trait DatalogPrinter extends DatalogBaseIRPrinter:
  override def prettyPrint(term: Term): String = term match
    case Block(at, t) if at.isEmpty => prettyPrint(t)
    case Block(at, t) => s"{${at.map(prettyPrint).mkString(", ")}; ${prettyPrint(t)}}"
    case _ => super.prettyPrint(term)
