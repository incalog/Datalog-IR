package inca.frontend.functional.typechecker

import inca.frontend.functional.syntax.*

object TypeUtil {
  def substitute(ty: Type, subst: Map[TName, Type]): Type = ty match {
    case TAny => TAny
    case TNothing => TNothing
    case tn@TName(_) => subst.getOrElse(tn, ty)
    case TFun(from, to) => TFun(from.map(substitute(_, subst)), substitute(to, subst))
    case TTuple(ts) => TTuple(ts.map(substitute(_, subst)))
    case TApply(name, tys) => TApply(name, tys.map(substitute(_, subst)))
    case TSet(ty) => TSet(substitute(ty, subst))
  }
}
