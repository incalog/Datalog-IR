package inca.ir.extension.set

import inca.ir.extension.bool.*
import inca.ir.typing.BaseIRTypechecker
import inca.ir.{Atom, TAny, TNothing, Term, Type}

trait Typechecker extends BaseIRTypechecker:
  override def subtype(ty1: Type, ty2: Type): Boolean = (ty1, ty2) match
    case (TSet(ty1), TSet(ty2)) => subtype(ty1, ty2)
    case (TSet(_), _) => false
    case (_, TSet(_)) => false
    case _ => super.subtype(ty1, ty2)

  override def typecheck(atom: Atom): Unit = atom match
    case SetMember(t1, t2) => (typecheck(t1), typecheck(t2)) match
      case (TSet(ty1), ty2) if !subtype(ty2, ty1) =>
        warn(s"Element of incompatible type $ty2 can not be contained in Set[$ty1]", atom)
      case (TSet(_), _) => // nothing
      case (ty1, _) =>
        error(s"Expected Set, but got $ty1")
    case _ => super.typecheck(atom)

  override def typecheckInternal(term: Term, inferred: Option[Type]): Type = term match
    case Set(ts) => TSet(join(ts.map(typecheck)))
    case SetIntersection(t1, t2) => (typecheck(t1), typecheck(t2)) match
      case (TSet(ty1), TSet(ty2)) if !subtype(ty1, ty2) && !subtype(ty1, ty2) =>
        warn(s"Set intersection between unrelated types: $ty1 and $ty2 will be empty.", term)
        TSet(TNothing)
      case (TSet(ty1), TSet(ty2)) =>
        TSet(join(ty1, ty2))
      case _ =>
        error(s"Expected sets, but got: $t1 ∩ $t2", term)
        TAny
    case SetUnion(t1, t2) => (typecheck(t1), typecheck(t2)) match
      case (TSet(ty1), TSet(ty2)) =>
        TSet(join(ty1, ty2))
      case _ =>
        error(s"Expected sets, but got: $t1 ∪ $t2", term)
        TAny
    case _ => super.typecheckInternal(term, inferred)
