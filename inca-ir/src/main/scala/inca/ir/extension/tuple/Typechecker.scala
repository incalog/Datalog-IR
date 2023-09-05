package inca.ir.extension.tuple

import inca.ir.extension.tuple.{Project, TTuple, Tuple}
import inca.ir.typing.BaseIRTypechecker
import inca.ir.{TAny, Term, Type}

trait Typechecker extends BaseIRTypechecker:
  override protected[ir] def typecheckInternal(term: Term, hint: Option[Type]): Type = term match {
    case Project(t, idx) => typecheckMust(t, None) match {
      case TTuple(tys) if idx < tys.size =>
        tys(idx)
      case TTuple(tys) =>
        error("Projection index out of bounds", t)
        TAny
      case ty =>
        error(s"Can not project on type: $ty", term)
        TAny
    }
    case Tuple(ts) => TTuple(ts.map(typecheck(_, None)))
    case _ => super.typecheckInternal(term, hint)
  }

  override protected[ir] def subtype(ty1: Type, ty2:  Type): Boolean = (ty1, ty2) match {
    case (TTuple(tys1), TTuple(tys2)) if tys1.size == tys2.size =>
      tys1.zip(tys2).forall { case (ty1, ty2)  => subtype(ty1, ty2) }
    case _ =>
      super.subtype(ty1, ty2)
  }

