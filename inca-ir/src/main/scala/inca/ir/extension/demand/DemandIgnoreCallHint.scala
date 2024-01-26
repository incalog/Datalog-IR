package inca.ir.extension.demand

import inca.ir.Hint
import inca.ir.HintKey

object DemandIgnoreCallHint extends Hint, HintKey[DemandIgnoreCallHint.type]:
  override def key: HintKey[_] = this
