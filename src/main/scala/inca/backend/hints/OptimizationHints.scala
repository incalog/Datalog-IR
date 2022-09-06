package inca.backend.hints

import inca.backend.hints.Hint.Key

object OptimizationHints {

  val NoInlineKey = "OPTIMIZE_NO_INLINE"

  /*
   * do not inline a pattern
   */
  case object NoInline extends Hint {
    override def key: Key = NoInlineKey
  }
}
