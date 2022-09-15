package inca.backend.hints

import inca.backend.hints.Hint.Key

object OptimizationHints {

  val NoInlineKey = "OPTIMIZE_NO_INLINE"

  //val IsExceptionKey = "OPTIMIZE_IS_EXCEPTION"

  /*
   * do not inline a pattern
   */
  case object NoInline extends Hint {
    override def key: Key = NoInlineKey
  }

  /*
   * mark an atom or pattern as used
   */
  /*case object IsException extends Hint {
    override def key: Key = IsExceptionKey
  }*/
}
