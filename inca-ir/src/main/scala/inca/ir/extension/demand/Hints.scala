package inca.ir.extension.demand

import inca.ir.Hint
import inca.ir.Hint.Key

object DemandIgnoreCallHintKey extends Hint.Key
object DemandIgnoreCallHint extends Hint:
  override def key: Key = DemandIgnoreCallHintKey
