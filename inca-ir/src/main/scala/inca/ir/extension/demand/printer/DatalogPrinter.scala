package inca.ir.extension.demand.printer

import inca.ir.extension.demand.TDemand
import inca.ir.Type
import inca.ir.printer.DatalogBaseIRPrinter

trait DatalogPrinter extends DatalogBaseIRPrinter:
  override def prettyPrint(ty: Type): String = ty match
    case TDemand(ty) => s"TDemand(${prettyPrint(ty)})"
    case _ => super.prettyPrint(ty)
