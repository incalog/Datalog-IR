package inca.frontend.extensions.boolOps

import inca.frontend.core.tree._
import inca.frontend.extensions.boolOps.Trees._
import inca.frontend.typechecker.CoreTypechecker

trait Typechecker extends CoreTypechecker {
  override protected def typecheckInternal(exp: Expression, anno: Option[Type]): Type = exp match {
    case Not(cond) =>
      val ty = typecheck(cond)
      if (!subtype(ty, TScalaBoolean, lang))
        error(s"Expected Boolean expression, but got $ty", cond)
      TScalaBoolean
    case And(e1, e2) =>
      val ty1 = typecheck(e1)
      val ty2 = typecheck(e2)
      if (!subtype(ty1, TScalaBoolean, lang))
        error(s"Expected Boolean expression, but got $ty1", e1)
      if (!subtype(ty2, TScalaBoolean, lang))
        error(s"Expected Boolean expression, but got $ty2", e2)
      TScalaBoolean
    case Or(e1, e2) =>
      val ty1 = typecheck(e1)
      val ty2 = typecheck(e2)
      if (!subtype(ty1, TScalaBoolean, lang))
        error(s"Expected Boolean expression, but got $ty1", e1)
      if (!subtype(ty2, TScalaBoolean, lang))
        error(s"Expected Boolean expression, but got $ty2", e2)
      TScalaBoolean
    case _ => super.typecheckInternal(exp, anno)
  }
}
