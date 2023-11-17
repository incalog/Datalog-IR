package inca.ir.extension.aggregate

import inca.ir.{Atom, Name, Param, Relation, TAny, Term, TermType, Type, Var}
import inca.ir.typing.{BaseIRTypechecker, Mode}

trait Typechecker extends BaseIRTypechecker:
  override def checkAtom(atom: Atom, mode: Mode): Unit = atom match
    case Aggregate(rel, args, op) =>
      val params = lookupRelationParams(rel, args.size, atom)
      val aggregands: Seq[Type] = args.zipAll(params, null, null).flatMap {
        case (AggregateArg.AggregateColumn(t), p) => // skip
          checkTerm(t, p.ty, mode)
          Some(p.ty)
        case (AggregateArg.Arg(t), null) => // missing param
          inferTerm(t, Mode.Bound)
          None
        case (null, p) => // missing argument
          None
        case (AggregateArg.Arg(t), p) =>
          checkTerm(t, p.ty, Mode.Bound)
          None
        case (AggregateArg.WildCard(t), p) =>
          checkTerm(t, p.ty, Mode.Binding)
          None
      }
      op.typecheck(aggregands) match
        case Left(err) =>
          error(err, atom)
          TAny.bound
        case Right(ty) =>
          ty.bound
    case _ => super.checkAtom(atom, mode)
