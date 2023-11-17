package inca.ir.extension.aggregate

import inca.ir.Hint.preserveHints
import inca.ir.extension.aggregate.AggregateArg.WildCard
import inca.ir.extension.not
import inca.ir.visitors.BaseIRVisitor
import inca.ir.{Atom, Term, Type}

trait Visitor extends BaseIRVisitor with not.Visitor:
  override def visitAtom(atom: Atom): Seq[Atom] = atom match
    case Aggregate(rel, args, op) =>
      val newargs = args.flatMap {
        case AggregateArg.Arg(t) => visitTerm(t).map(AggregateArg.Arg.apply)
        case AggregateArg.AggregateColumn(t) =>
          val Seq(t0) = visitTerm(t)
          Seq(AggregateArg.AggregateColumn(t0))
        case AggregateArg.WildCard(t) =>
          val Seq(t0) = visitTerm(t)
          Seq(WildCard(t0))
      }
      Seq(Aggregate(rel, newargs, op))
    case _ => super.visitAtom(atom)
