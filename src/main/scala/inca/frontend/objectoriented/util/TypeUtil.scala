package inca.frontend.objectoriented.util

import inca.frontend.objectoriented.core.{TAny, TFun, TClass, TName, TNothing, TOption, Type}

object TypeUtil {
  def substitute(ty: Type, subst: Map[TName, Type]): Type = ty match {
    case TAny => TAny
    case TNothing => TNothing
    case tn@TName(_) => subst.getOrElse(tn, ty)
    // TODO: Figure out what this is needed for
    //case TClass(name) =>
    case TFun(from, to) => TFun(from.map(substitute(_, subst)), substitute(to, subst))
    case TOption(ty) => TOption(substitute(ty, subst))
  }
}
