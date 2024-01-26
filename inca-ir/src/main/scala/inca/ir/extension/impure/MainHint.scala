package inca.ir.extension.impure

import inca.ir.{Hint, HintKey}

object MainHint extends Hint, HintKey[MainHint.type]:
  override def key: HintKey[_] = this

