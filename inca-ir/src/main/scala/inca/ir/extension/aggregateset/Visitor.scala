package inca.ir.extension.aggregateset

import inca.ir.Hint.preserveHints
import inca.ir.extension.aggregate.AggregateArg
import inca.ir.extension.not
import inca.ir.visitors.BaseIRVisitor
import inca.ir.{Atom, Term, Type}

trait Visitor extends BaseIRVisitor with not.Visitor:
  override def visitAtom(atom: Atom): Seq[Atom] = atom match
    case AggregateSet(rel, args, op) =>
      val newargs = args.flatMap {
        case AggregateArg.Arg(t) => visitTerm(t).map(AggregateArg.Arg.apply)
        case AggregateArg.AggregateColumn(t) =>
          val Seq(t0) = visitTerm(t)
          Seq(AggregateArg.AggregateColumn(t0))
      }
      Seq(AggregateSet(rel, newargs, op))
    case _ => super.visitAtom(atom)
