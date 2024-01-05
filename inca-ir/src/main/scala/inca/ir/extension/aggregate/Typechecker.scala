package inca.ir.extension.aggregate

import inca.ir.{Atom, IRelation, Name, Param, Relation, TAny, Term, TermArg, TermType, Type, Var, WildcardArg}
import inca.ir.typing.{BaseIRTypechecker, Mode}

trait Typechecker extends BaseIRTypechecker:
  override def checkAtom(atom: Atom, mode: Mode): Unit = atom match
    case Aggregate(ref, args, op) =>
      val paramTys = inferRelationRef(ref.as[IRelation], atom)
      if (paramTys.size != args.size)
        error(s"Expected ${paramTys.size} arguments but got: ${args.size}", atom)

      val aggregands: Seq[Type] = args.zipAll(paramTys, null, null).flatMap {
        case (AggregateColumnArg(t), pty) => // skip
          checkTerm(t,op.resultType, mode)
          Some(pty)
        case (TermArg(t), null) => // missing param
          inferTerm(t, Mode.Bound)
          None
        case (null, pty) => // missing argument
          None
        case (TermArg(t), pty) =>
          checkTerm(t, pty, Mode.Bound)
          None
        case (wildcard@WildcardArg(), pty) =>
          wildcard.typed(pty.collapsed, force = true)
          None
      }
      op.typecheck(aggregands).foreach(error(_, atom))
      op.resultType.bound
    case _ => super.checkAtom(atom, mode)
