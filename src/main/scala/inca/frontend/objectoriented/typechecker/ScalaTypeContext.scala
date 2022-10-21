package inca.frontend.objectoriented.typechecker

import inca.frontend.objectoriented.core.{TAny, TClass, TNull, TScala, TSet, TTuple, Type}
import inca.util.ScalaTyper

trait ScalaTypeContext extends TypeContext with ScalaTyper {
  override def scopedTypeContext[T](f: => T): T = {
    val prevSeenCode = seenCode
    val prevImports = imports
    val prevBoundNames = importBoundNames
    val prevToplevelBoundNames = toplevelBoundNames
    val t = super.scopedTypeContext(f)
    seenCode = prevSeenCode
    imports = prevImports
    importBoundNames = prevBoundNames
    toplevelBoundNames = prevToplevelBoundNames
    t
  }

  override def subtype(ty1: Type, ty2: Type): Boolean = (ty1, ty2) match {
    case (TScala(s1), TScala(s2)) => subtypeScala(s1.tree, s2.tree)
    case (_, TScala(s2)) => subtypeScala(ty1.asScala, s2.tree)
    case (TScala(s1), _) => subtypeScala(s1.tree, ty2.asScala)
    case _ => super.subtype(ty1, ty2)
  }
}
