package inca.backend.hints

object MagicSetHints {

  val MainKey = "MAGIC_MAIN"
  val IgnoreCallkey = "MAGIC_IGNORE_CALL"
  val FixedAdornmentKey = "MAGIC_FIXED_ADORNMENT"
  val NoInputRelationKey = "MAGIC_NO_INPUT_RELATION"

  /**
   * marks calls from which the magic set transformation starts
   */
  case class Main(adorn: Seq[Boolean]) extends Hint {
    val key = MainKey
  }

  /**
   * ignore this call when collecting the inputs of the called pattern
   */
  object IgnoreCall extends Hint {
    val key = IgnoreCallkey
  }

  /**
   * use the given adornment instead of computing it
   */
  case class FixedAdornment(adorn: Seq[Boolean]) extends Hint {
    val key = IgnoreCallkey
  }

  /**
   * do not derive input relation for this pattern
   */
  object NoInputRelation extends Hint {
    val key = NoInputRelationKey
  }
}
