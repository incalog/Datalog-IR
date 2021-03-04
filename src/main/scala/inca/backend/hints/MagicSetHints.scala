package inca.backend.hints

import inca.backend.hints.Hint.Key

object MagicSetHints {

  val MainKey = "MAGIC_MAIN"
  val IgnoreCallKey = "MAGIC_IGNORE_CALL"
  val FixedAdornmentKey = "MAGIC_FIXED_ADORNMENT"
  val NoInputRelationKey = "MAGIC_NO_INPUT_RELATION"
  val AdornmentKey = "MAGIC_ADORNMENT"
  val InputCallKey = "MAGIC_INPUT_CALL"

  /**
   * marks calls from which the magic set transformation starts
   */
  case class Main(adorn: Seq[Boolean]) extends Hint {
    val key: Key = MainKey
  }

  /**
   * ignore this call when collecting the inputs of the called pattern
   */
  case object IgnoreCall extends Hint {
    val key: Key = IgnoreCallKey
  }

  /**
   * use the given adornment instead of computing it
   */
  case class FixedAdornment(adorn: Seq[Boolean]) extends Hint {
    val key: Key = FixedAdornmentKey
  }

  /**
   * do not derive input relation for this pattern or body
   */
  case object NoInputRelation extends Hint {
    val key: Key = NoInputRelationKey
  }

  /**
   * indicates the adornment
   */
  case class Adornment(adorn: Seq[Boolean]) extends Hint {
    val key: Key = AdornmentKey
  }

  case class InputCall(name: String) extends Hint {
    override def key: Key = InputCallKey
  }
}
