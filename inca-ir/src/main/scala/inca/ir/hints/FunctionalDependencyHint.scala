package inca.ir.hints

import inca.ir.Hint.Key
import inca.ir.{Hint, Name}

case class FunctionalDependencyHint(values: Seq[Name], determine: Seq[Name]) extends Hint:
  override def key: Key = FunctionalDependencyHint

object FunctionalDependencyHint extends Hint.Key
  


