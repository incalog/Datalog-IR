package org.inca.diff.diffable.derive

import org.inca.diff.diffable.{Diffable, MetaVar, Change}
import org.inca.diff.diffable.macros.diffableType

@diffableType
trait Tree23


object test {
  def isDiffable(t: Tree23): Diffable[Tree23] = t

  val varHole = Tree23.VarHole(new MetaVar[Tree23](5))
  val changeHole = Tree23.ChangeHole(Change[Tree23](varHole, varHole))
}

