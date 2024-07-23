package inca.ir.extension.set

import inca.ir.extension.bool.*
import inca.ir.extension.tuple.TTuple
import inca.ir.typing.{BaseIRTypechecker, Mode}
import inca.ir.{Atom, Relation, TAny, TNothing, Term, TermType, Type}

trait Typechecker extends BaseIRTypechecker:
  protected override def inferTermExtend(term: Term, mode: Mode): TermType = term match
    case SetLit(Seq()) =>
      TSet(TNothing).bound
    case SetLit(t +: ts) =>
      val ty = inferTerm(t, Mode.Bound).ty
      ts.foreach(checkTerm(_, ty, Mode.Bound))
      TSet(ty).bound
    case SetFrom(ref) =>
      inferRelationRef(ref) match
        case Seq(ty) => TSet(ty).bound
        case tys => TSet(TTuple(tys)).bound
        //case _ =>
        //  error(s"Cannot find relation $name", term)
        //  TSet(TAny).bound
    case SetIntersection(t1, t2) =>
      val (TSet(ty1), m1) = inferSetTerm(t1, Mode.Bound)
      val (TSet(ty2), m2) = inferSetTerm(t2, Mode.Bound)
      assertComparable(ty2, ty1, term)
      TermType(TSet(ty1), m1 || m2)
    case SetUnion(t1, t2) =>
      val (TSet(ty1), m1) = inferSetTerm(t1, Mode.Bound)
      val (TSet(ty2), m2) = inferSetTerm(t2, Mode.Bound)
      assertComparable(ty2, ty1, term)
      TermType(TSet(ty1), m1 || m2)
    case SetComprehension(elem, atoms) => scopedTypeContext {
      atoms.foreach(checkAtom(_, Mode.Binding))
      val TermType(ty, m) = inferTerm(elem, mode)
      TermType(TSet(ty), m)
    }
    case _ => super.inferTermExtend(term, mode)

  private def inferSetTerm(t: Term, mode: Mode): (TSet,Mode) = inferTerm(t, mode) match
    case TermType(ty: TSet, m) => (ty, m)
    case TermType(ty, m) =>
      error(s"Expected set type but got $ty", t)
      (TSet(TNothing), m)

  protected override def checkAtom(atom: Atom, mode: Mode): Unit = atom match
    case SetMember(mem, s) =>
      val (TSet(ty),_) = inferSetTerm(s, Mode.Bound)
      checkTerm(mem, ty, mode)
    case _ => super.checkAtom(atom, mode)

  override def checkType(ty: Type): Unit = ty match
    case TSet(tty) => checkType(tty)
    case _ => super.checkType(ty)
