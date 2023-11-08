package inca.ir.extension.impure

import inca.ir.Hint
import inca.ir.Hint.Key
import inca.ir.Var

object Hints:
  val PureKey = "PURE"

  object Pure extends Hint:
    override def key: Key = PureKey

