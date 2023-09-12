package inca.ir.extension.set

import inca.ir.extension.bool.*
import inca.ir.typing.{BaseIRTypechecker, Mode}
import inca.ir.{Atom, TAny, TNothing, Term, Type}

trait Typechecker extends BaseIRTypechecker:
  override def subtype(ty1: Type, ty2: Type): Boolean = (ty1, ty2) match
      case (TSet(ty1), TSet(ty2)) => subtype(ty1, ty2)
      case (TSet(_), _) => false
      case (_, TSet(_)) => false
      case _ => super.subtype(ty1, ty2)

  protected override def inferTermExtend(term: Term, mode: Mode): Type = term match
    case Set(ts) => TSet(join(ts.map(inferTerm(_, Mode.Closed))))
    case SetIntersection(t1, t2) =>
      val TSet(ty1) = inferSetTerm(t1, mode)
      val TSet(ty2) = inferSetTerm(t2, mode)
      assertComparable(ty2, ty1, term)
      TSet(meet(ty1, ty2))
    case SetUnion(t1, t2) =>
      val TSet(ty1) = inferSetTerm(t1, mode)
      val TSet(ty2) = inferSetTerm(t2, mode)
      TSet(join(ty1, ty2))
    case _ => super.inferTermExtend(term, mode)

  private def inferSetTerm(t: Term, mode: Mode): TSet = inferTerm(t, mode) match
    case ty: TSet => ty
    case ty =>
      error(s"Expected set type but got $ty", t)
      TSet(TNothing)

  override def checkAtom(atom: Atom, mode: Mode): Unit = atom match
    case SetMember(mem, s) =>
      val TSet(ty) = inferSetTerm(s, Mode.Closed)
      checkTerm(mem, ty, mode)
    case _ => super.checkAtom(atom, mode)
