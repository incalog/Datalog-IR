package inca.backend.hints

object MagicSetHints {
  /**
   * ignore this call when collecting the inputs of the called pattern
   */
  object IgnoreCall extends Hint {
    val key = "MAGIC_IGNORE_CALL"
  }

  /**
   * use the given adornment instead of computing it
   */
  case class FixedAdornment(adorn: Seq[Boolean]) extends Hint {
    val key = "MAGIC_FIXED_ADORNMENT"
  }

  /**
   * do not derive input relation for this pattern
   */
  object NoInputRelation extends Hint {
    val key = "MAGIC_NO_INPUT_RELATION"
  }
}
