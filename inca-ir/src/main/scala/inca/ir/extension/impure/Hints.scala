package inca.ir.extension.impure

import inca.ir.Hint
import inca.ir.Hint.Key
import inca.ir.Var

object PureHintKey extends Hint.Key
object PureHint extends Hint:
  override def key: Key = PureHintKey

