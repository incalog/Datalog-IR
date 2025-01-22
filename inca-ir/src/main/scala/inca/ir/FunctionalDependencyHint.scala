package inca.ir

import inca.ir.Hint.Key
import inca.ir.{Hint, Var}

case class FunctionalDependencyHint(values: Seq[Name], determine: Seq[Name]) extends Hint:
  override def key: Key = FunctionalDependencyHint

object FunctionalDependencyHint extends Hint.Key
  


