package inca.ir

import inca.ir.Hint.Key
import inca.ir.{Hint, Var}

object MainHint extends Hint, Hint.Key:
  override def key: Key = this

