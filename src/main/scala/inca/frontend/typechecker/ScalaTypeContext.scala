package inca.frontend.typechecker

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
}

