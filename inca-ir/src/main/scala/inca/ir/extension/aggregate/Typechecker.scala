package inca.ir.extension.aggregate

import inca.ir.{Atom, Param, Relation, TAny, Term, TermType, Type}
import inca.ir.typing.{BaseIRTypechecker, Mode}

trait Typechecker extends BaseIRTypechecker:
  override def checkAtom(atom: Atom, mode: Mode): Unit = atom match
    case Aggregate(rel, args, op) =>
      val params = lookupRelationParams(rel, args.size, atom)
      val argMode = mode match
        case Mode.Binding => Mode.Bound
        case Mode.Bound => Mode.Collapse
        case Mode.Collapse => Mode.Collapse
      val aggregands = args.zipAll(params, null, null).flatMap {
        case (AggregateArg.AggregateColumn(t), p) => // skip
          checkTerm(t, p.ty, mode)
          Some(p.ty)
        case (AggregateArg.Arg(t), null) => // missing param
          inferTerm(t, argMode)
          None
        case (null, p) => // missing argument
          None
        case (AggregateArg.Arg(t), p) =>
          checkTerm(t, p.ty, argMode)
          None
      }
      op.typecheck(aggregands) match
        case Left(err) =>
          error(err, atom)
          TAny.bound
        case Right(ty) =>
          ty.bound
    case _ => super.checkAtom(atom, mode)
