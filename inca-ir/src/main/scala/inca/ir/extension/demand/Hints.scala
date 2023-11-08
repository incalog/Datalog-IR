package inca.ir.extension.demand

import inca.ir.Hint
import inca.ir.Hint.Key

object Hints:
  val IgnoreCallKey = "NO_INPUT"

  object IgnoreCall extends Hint:
    override def key: Key = IgnoreCallKey
