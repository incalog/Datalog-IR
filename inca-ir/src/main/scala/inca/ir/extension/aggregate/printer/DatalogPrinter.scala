package inca.ir.extension.aggregate.printer

import inca.ir.extension.aggregate.{AggregateColumnArg, Aggregate}
import inca.ir.{Arg, Atom}
import inca.ir.printer.DatalogBaseIRPrinter

trait DatalogPrinter extends DatalogBaseIRPrinter:
  override def prettyPrint(arg: Arg): String = arg match
    case AggregateColumnArg(t) => s"#${prettyPrint(t)}"
    case _ => super.prettyPrint(arg)

  override def prettyPrint(atom: Atom): String = atom match
    case Aggregate(rel, args, op) =>
      s"aggregate(${prettyPrint(rel)}(${args.map(prettyPrint).mkString(", ")}), $op)"
    case _ => super.prettyPrint(atom)
