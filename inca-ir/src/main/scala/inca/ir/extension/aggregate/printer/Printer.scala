package inca.ir.extension.aggregate.printer

import inca.ir.extension.aggregate.{Aggregate, AggregateColumnArg, AggregationOperator}
import inca.ir.{Arg, Atom}
import inca.ir.printer.BaseIRPrinter

trait Printer extends BaseIRPrinter:
  def prettyPrint(op: AggregationOperator): String = op.toString

  override def prettyPrint(arg: Arg): String = arg match
    case AggregateColumnArg(t) => s"#${prettyPrint(t)}"
    case _ => super.prettyPrint(arg)

  override def prettyPrint(atom: Atom): String = atom match
    case Aggregate(rel, args, op) =>
      s"aggregate(${prettyPrint(rel)}(${args.map(prettyPrint).mkString(", ")}), ${prettyPrint(op)})"
    case _ => super.prettyPrint(atom)
