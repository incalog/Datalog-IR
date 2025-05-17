package inca.ir.extension.aggregategeneric.printer

import inca.ir.extension.aggregategeneric.AggregateGeneric
import inca.ir.{Arg, Atom, Term}
import inca.ir.printer.BaseIRPrinter
import inca.ir.extension.aggregate.printer.{Printer as AggregatePrinter}

trait Printer extends BaseIRPrinter with AggregatePrinter:

  override def prettyPrint(term: Term): String = term match
    case AggregateGeneric(agg, ats, op) =>
      if (agg.isDefined)
        s"${prettyPrint(op)} ${prettyPrint(agg.get)} : { ${ats.map(prettyPrint).mkString(", ")} }"
      else
        s"${prettyPrint(op)} : { ${ats.map(prettyPrint).mkString(", ")} }"
    case _ => super.prettyPrint(term)
