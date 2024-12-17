package inca.ir.extension.foreign.printer

import inca.ir.extension.foreign.{ConvertForeignIR, ConvertIRForeign}
import inca.ir.Term
import inca.ir.printer.DatalogBaseIRPrinter

trait DatalogPrinter extends DatalogBaseIRPrinter:
  override def prettyPrint(term: Term): String = term match
    case ConvertForeignIR(term, foreignType, irType) => s"${prettyPrint(term)} as ${prettyPrint(irType)}"
    case ConvertIRForeign(term, irType, foreignType) => s"${prettyPrint(term)} as ${prettyPrint(foreignType)}"
    case _ => super.prettyPrint(term)
