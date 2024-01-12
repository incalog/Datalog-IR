package inca.ir.extension.impure

import inca.ir.Hint
import inca.ir.Hint.Key
import inca.ir.Var

object MainHintKey extends Hint.Key
object MainHint extends Hint:
  override def key: Key = MainHintKey

