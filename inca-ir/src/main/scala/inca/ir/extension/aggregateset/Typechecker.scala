package inca.ir.extension.aggregateset

import inca.ir.typing.{BaseIRTypechecker, Mode}
import inca.ir.*
import inca.ir.extension.aggregate.AggregateArg
import inca.ir.extension.set.TSet

trait Typechecker extends BaseIRTypechecker:
  override def checkAtom(atom: Atom, mode: Mode): Unit = atom match
    case AggregateSet(rel, args, op) =>
      val params = lookupRelationParams(rel, args.size, atom)
      val argMode = mode match
        case Mode.Binding => Mode.Bound
        case Mode.Bound => Mode.Collapse
        case Mode.Collapse => Mode.Collapse
      val aggregands = args.zipAll(params, null, null).flatMap {
        case (AggregateArg.AggregateColumn(t), p) => // skip
          val ty = p.ty match
            case TSet(ty) => ty
            case ty => error(s"aggregation column should have set type, but was $ty", atom); ty
          checkTerm(t, op.resultType, mode)
          Some(ty)
        case (AggregateArg.Arg(t), null) => // missing param
          inferTerm(t, argMode)
          None
        case (null, p) => // missing argument
          None
        case (AggregateArg.Arg(t), p) =>
          checkTerm(t, p.ty, argMode)
          None
        // TODO case AggregateArg.WildCard
      }
      op.typecheck(aggregands).foreach(error(_, atom))
      op.resultType
    case _ => super.checkAtom(atom, mode)
