package inca.ir.hints

import inca.ir.Hint.Key
import inca.ir.{Hint, Name}

case class FoldHint(resultParamName: Name) extends Hint:
  override def key: Key = FoldHint

object FoldHint extends Hint.Key
  


