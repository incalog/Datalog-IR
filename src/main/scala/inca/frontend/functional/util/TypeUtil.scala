package inca.frontend.functional.util

import inca.frontend.functional.core.{TAny, TConstr, TFun, TName, TNothing, TOption, TScala, TSet, TTuple, Type}

object TypeUtil {
  def substitute(ty: Type, subst: Map[TName, Type]): Type = ty match {
    case TAny => TAny
    case TNothing => TNothing
    case TScala(ty) => TScala(ty)
    case tn@TName(_) => subst.getOrElse(tn, ty)
    case TFun(from, to) => TFun(from.map(substitute(_, subst)), substitute(to, subst))
    case TTuple(ts) => TTuple(ts.map(substitute(_, subst)))
    case TConstr(name, tys) => TConstr(name, tys.map(substitute(_, subst)))
    case TOption(ty) => TOption(substitute(ty, subst))
    case TSet(ty) => TSet(substitute(ty, subst))
  }
}
