package inca.frontend.oodl.typechecker

import inca.frontend.oodl.syntax.*

object TypeUtil {
  def substitute(ty: Type, subst: Map[TName, Type]): Type = ty match {
    case TAny => TAny
    case tn@TName(_, _) => subst.getOrElse(tn, ty)
    case TTuple(ts) => TTuple(ts.map(substitute(_, subst)))
    case TSet(ty) => TSet(substitute(ty, subst))
  }
}
