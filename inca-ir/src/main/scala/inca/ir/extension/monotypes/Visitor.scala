package inca.ir.extension.monotypes

import inca.ir.{Atom, Term, TermArg, Type}
import inca.ir.visitors.BaseIRVisitor
import inca.ir.Hint.preserveHints
import inca.ir.extension.aggregate.{Aggregate, AggregateColumnArg}

import scala.collection.immutable.Seq

trait Visitor extends BaseIRVisitor {
  override def visitTerm(term: Term): Seq[Term] = preserveHints(term)(term match
    case ResultMono(m) =>
      Seq(ResultMono(m))
    case MkMono(mono, args, keys) =>
      val visitedArgs : Seq[Term] = args.flatMap{arg => visitTerm(arg)}
      val visitedKeys : Seq[Type] = keys.map(visitType)
      Seq(MkMono(mono, visitedArgs, visitedKeys))
    case MonoAggregate(rel, args, op) =>
      val newargs = args.flatMap {
        case TermArg(t) => visitTerm(t).map(TermArg.apply)
        case AggregateColumnArg(t) =>
          val Seq(t0) = visitTerm(t)
          Seq(AggregateColumnArg(t0))
      }
      Seq (MonoAggregate(rel, newargs, op))
    case _ => super.visitTerm(term)
  )

  override def visitAtom(atom: Atom): Seq[Atom] = preserveHints(atom)(atom match
    case AddMono(m, input, keys) =>
      val visitedM : Term = visitTerm(m).head
      val visitedInput : Term = visitTerm(input).head
      val visitedKey : Seq[Term] = keys.flatMap(visitTerm)
      Seq(AddMono(visitedM, visitedInput, visitedKey))
    case _ => super.visitAtom(atom)
  )

  override def visitType(ty: Type): Type = preserveHints(ty)(ty match
    case TMono(input, output, keys) =>
      TMono(visitType(input), visitType(output), keys.map(visitType))
    case _ => super.visitType(ty)
  )
}
