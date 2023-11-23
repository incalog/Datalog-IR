package inca.ir.extension.aggregate

import inca.ir.Hint.preserveHints
import inca.ir.extension.not
import inca.ir.visitors.BaseIRVisitor
import inca.ir.{Atom, Term, TermArg, Type, WildcardArg}

trait Visitor extends BaseIRVisitor with not.Visitor:
  override def visitAtom(atom: Atom): Seq[Atom] = atom match
    case Aggregate(rel, args, op) =>
      val newargs = args.flatMap {
        case TermArg(t) => visitTerm(t).map(TermArg.apply)
        case AggregateColumnArg(t) =>
          val Seq(t0) = visitTerm(t)
          Seq(AggregateColumnArg(t0))
        case WildcardArg =>
          Seq(WildcardArg)
      }
      Seq(Aggregate(rel, newargs, op))
    case _ => super.visitAtom(atom)
