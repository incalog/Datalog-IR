package inca.ir.extension.foreign.printer

import inca.ir.extension.foreign.{ConvertForeignIR, ConvertIRForeign, ForeignAggregationOperator}
import inca.ir.Term
import inca.ir.extension.aggregate.AggregationOperator
import inca.ir.extension.aggregate.printer.Printer as AggPrinter
import inca.ir.printer.BaseIRPrinter

trait Printer
  extends BaseIRPrinter
  with AggPrinter:

  override def prettyPrint(op: AggregationOperator): String = op match
    case fop: ForeignAggregationOperator => s"${fop.lang}.${fop.name}"
    case _ => super.prettyPrint(op)

  override def prettyPrint(term: Term): String = term match
    case ConvertForeignIR(term, foreignType, irType) => s"${prettyPrint(term)} as ${prettyPrint(irType)}"
    case ConvertIRForeign(term, irType, foreignType) => s"${prettyPrint(term)} as ${prettyPrint(foreignType)}"
    case _ => super.prettyPrint(term)
