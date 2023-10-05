package inca.ir.extension.set

import inca.ir.extension.bool.*
import inca.ir.extension.tuple.TTuple
import inca.ir.typing.{BaseIRTypechecker, Mode}
import inca.ir.{Atom, Relation, TAny, TNothing, Term, TermType, Type}

trait Typechecker extends BaseIRTypechecker:
  override protected def join(ty1: Type, ty2: Type): Type = (ty1, ty2) match
    case (TSet(tty1), TSet(tty2)) => TSet(join(tty1, tty2))
    case _ => super.join(ty1, ty2)

  override protected def meet(ty1: Type, ty2: Type): Type = (ty1, ty2) match
    case (TSet(tty1), TSet(tty2)) => TSet(meet(tty1, tty2))
    case _ => super.meet(ty1, ty2)

  protected override def inferTermExtend(term: Term, mode: Mode): TermType = term match
    case SetLit(ts) =>
      val tys = ts.map(inferTerm(_, Mode.Bound).ty)
      TSet(joinTypes(tys)).closed
    case SetRef(name) =>
      lookupModuleEntry(name) match
        case Some(Relation(_, params, _)) =>
          val tys = params.map(_.ty)
          if (tys.size == 1)
            TSet(tys.head).closed
          else
            TSet(TTuple(tys)).closed
        case _ =>
          error(s"Cannot find relation $name", term)
          TSet(TAny).closed
    case SetIntersection(t1, t2) =>
      val (TSet(ty1), m1) = inferSetTerm(t1, Mode.Bound)
      val (TSet(ty2), m2) = inferSetTerm(t2, Mode.Bound)
      assertComparable(ty2, ty1, term)
      TermType(TSet(meet(ty1, ty2)), m1 || m2)
    case SetUnion(t1, t2) =>
      val (TSet(ty1), m1) = inferSetTerm(t1, Mode.Bound)
      val (TSet(ty2), m2) = inferSetTerm(t2, Mode.Bound)
      TermType(TSet(join(ty1, ty2)), m1 || m2)
    case SetComprehension(elem, atoms) =>
      atoms.foreach(checkAtom(_, Mode.Binding))
      val TermType(ty, m) = inferTerm(elem, mode)
      TermType(TSet(ty), m)
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
