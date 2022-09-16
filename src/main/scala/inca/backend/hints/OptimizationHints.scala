package inca.backend.hints

import inca.backend.hints.Hint.Key

object OptimizationHints {

  val NoInlineKey = "OPTIMIZE_NO_INLINE"

  val NoInlineInputKey = "OPTIMIZE_NO_INLINE_INPUT"

  //val IsExceptionKey = "OPTIMIZE_IS_EXCEPTION"

  /*
   * do not inline a pattern
   */
  case object NoInline extends Hint {
    override def key: Key = NoInlineKey
  }

  /*
   * do not inline the corresponding input pattern
   */
  case object NoInlineInput extends Hint {
    override def key: Key = NoInlineInputKey
  }

  /*
   * mark an atom or pattern as used
   */
  /*case object IsException extends Hint {
    override def key: Key = IsExceptionKey
  }*/
}
