package inca.ir.extension.aggregateset.printer

import inca.ir.extension.aggregateset.AggregateSet
import inca.ir.{Arg, Atom}
import inca.ir.printer.BaseIRPrinter

trait Printer extends BaseIRPrinter:

  override def prettyPrint(atom: Atom): String = atom match
    case AggregateSet(rel, args, op) =>
      s"aggregateSet(${prettyPrint(rel)}(${args.map(prettyPrint).mkString(", ")}), $op)"
    case _ => super.prettyPrint(atom)
