package inca.ir.extension.impure

import inca.ir.Hint
import inca.ir.Hint.Key
import inca.ir.Var

object MainHint extends Hint, Hint.Key:
  override def key: Key = this

