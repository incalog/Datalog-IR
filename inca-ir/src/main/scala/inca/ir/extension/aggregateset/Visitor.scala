package inca.ir.extension.aggregateset

import inca.ir.Hint.preserveHints
import inca.ir.extension.aggregate.AggregateColumnArg
import inca.ir.extension.not
import inca.ir.visitors.BaseIRVisitor
import inca.ir.{Atom, Term, TermArg, Type}

trait Visitor extends BaseIRVisitor with not.Visitor:
  override def visitAtom(atom: Atom): Seq[Atom] = atom match
    case AggregateSet(rel, args, op) =>
      val newargs = args.flatMap {
        case TermArg(t) => visitTerm(t).map(TermArg.apply)
        case AggregateColumnArg(t) =>
          val Seq(t0) = visitTerm(t)
          Seq(AggregateColumnArg(t0))
      }
      Seq(AggregateSet(rel, newargs, op))
    case _ => super.visitAtom(atom)
