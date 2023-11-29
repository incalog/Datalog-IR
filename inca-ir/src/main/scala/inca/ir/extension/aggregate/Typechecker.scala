package inca.ir.extension.aggregate

import inca.ir.{Atom, Name, Param, Relation, TAny, Term, TermArg, TermType, Type, Var, WildcardArg}
import inca.ir.typing.{BaseIRTypechecker, Mode}

trait Typechecker extends BaseIRTypechecker:
  override def checkAtom(atom: Atom, mode: Mode): Unit = atom match
    case Aggregate(rel, args, op) =>
      val params = lookupRelationParams(rel, args.size, atom)
      val aggregands: Seq[Type] = args.zipAll(params, null, null).flatMap {
        case (AggregateColumnArg(t), p) => // skip
          checkTerm(t,op.resultType, mode)
          Some(p.ty)
        case (TermArg(t), null) => // missing param
          inferTerm(t, Mode.Bound)
          None
        case (null, p) => // missing argument
          None
        case (TermArg(t), Param(_, ty)) =>
          checkTerm(t, ty, Mode.Bound)
          None
        case (wildcard@WildcardArg(), Param(_, ty)) =>
          wildcard.typed(ty.collapsed, force = true)
          None
        case (WildcardArg(), _) =>
          None
      }
      op.typecheck(aggregands).foreach(error(_, atom))
      op.resultType.bound
    case _ => super.checkAtom(atom, mode)
