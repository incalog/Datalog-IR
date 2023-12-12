package inca.ir.extension.aggregate

import inca.ir.Hint.preserveHints
import inca.ir.extension.not
import inca.ir.visitors.BaseIRVisitor
import inca.ir.{Arg, Atom, Term, TermArg, Type, WildcardArg}

trait Visitor extends BaseIRVisitor with not.Visitor:
  override def visitArg(arg: Arg): Seq[Arg] = arg match
    case AggregateColumnArg(t) =>
      val Seq(t0) = visitTerm(t)
      Seq(AggregateColumnArg(t0))
    case _ => super.visitArg(arg)

  override def visitAtom(atom: Atom): Seq[Atom] = atom match
    case Aggregate(rel, args, op) =>
      Seq(Aggregate(rel, args.flatMap(visitArg), op))
    case _ => super.visitAtom(atom)
