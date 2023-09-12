package inca.ir.extension.tuple

import inca.ir.extension.tuple.{Project, TTuple, Tuple}
import inca.ir.typing.{BaseIRTypechecker, Mode}
import inca.ir.{TAny, Term, Type}

trait Typechecker extends BaseIRTypechecker:
  override protected[ir] def inferTermExtend(term: Term, mode: Mode): Type = term match {
    case Tuple(ts) => TTuple(ts.map(inferTerm(_, mode)))
    case Project(t, idx) => inferTerm(t, Mode.Closed) match {
      case TTuple(tys) if 0 <= idx && idx < tys.size => tys(idx)
      case TTuple(tys) =>
        error("Projection index out of bounds", term)
        TAny
      case ty =>
        error(s"Expected tuple type but got $ty", term)
        TAny
    }
    case _ => super.inferTermExtend(term, mode)
  }

  override protected[ir] def subtype(ty1: Type, ty2:  Type): Boolean = (ty1, ty2) match {
    case (TTuple(tys1), TTuple(tys2)) if tys1.size == tys2.size =>
      tys1.zip(tys2).forall { case (ty1, ty2)  => subtype(ty1, ty2) }
    case _ =>
      super.subtype(ty1, ty2)
  }

