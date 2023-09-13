package inca.ir.extension.set

import inca.ir.extension.bool.*
import inca.ir.typing.{BaseIRTypechecker, Mode}
import inca.ir.{Atom, TAny, TNothing, Term, TermType, Type}

trait Typechecker extends BaseIRTypechecker:
  override protected def join(ty1: Type, ty2: Type): Type = (ty1, ty2) match
    case (TSet(tty1), TSet(tty2)) => TSet(join(tty1, tty2))
    case _ => super.join(ty1, ty2)

  override protected def meet(ty1: Type, ty2: Type): Type = (ty1, ty2) match
    case (TSet(tty1), TSet(tty2)) => TSet(meet(tty1, tty2))
    case _ => super.meet(ty1, ty2)

  protected override def inferTermExtend(term: Term, mode: Mode): TermType = term match
    case Set(ts) =>
      val tys = ts.map(inferTerm(_, Mode.Bound).ty)
      TSet(joinTypes(tys)).closed
    case SetIntersection(t1, t2) =>
      val (TSet(ty1), m1) = inferSetTerm(t1, mode)
      val (TSet(ty2), m2) = inferSetTerm(t2, mode)
      assertComparable(ty2, ty1, term)
      TermType(TSet(meet(ty1, ty2)), m1 || m2)
    case SetUnion(t1, t2) =>
      val (TSet(ty1), m1) = inferSetTerm(t1, mode)
      val (TSet(ty2), m2) = inferSetTerm(t2, mode)
      TermType(TSet(join(ty1, ty2)), m1 || m2)
    case _ => super.inferTermExtend(term, mode)

  private def inferSetTerm(t: Term, mode: Mode): (TSet,Mode) = inferTerm(t, mode) match
    case TermType(ty: TSet, m) => (ty, m)
    case TermType(ty, m) =>
      error(s"Expected set type but got $ty", t)
      (TSet(TNothing), m)

  override def checkAtom(atom: Atom, mode: Mode): Unit = atom match
    case SetMember(mem, s) =>
      val (TSet(ty),_) = inferSetTerm(s, Mode.Bound)
      checkTerm(mem, ty, mode)
    case _ => super.checkAtom(atom, mode)
