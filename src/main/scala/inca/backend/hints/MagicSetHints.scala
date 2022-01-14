package inca.backend.hints

import inca.backend.hints.Hint.Key

object MagicSetHints {

  /**
   * marks calls from which the magic set transformation starts
   */
  object Main {
    val key: Key = "MAGIN_MAIN"
  }
  case class Main(adorn: Seq[Boolean]) extends Hint {
    val key: Key = Main.key
  }

  /**
   * indicates the demand patterns for a pattern
   */
  object DemandPatterns {
    val key: Key = "DEMAND_DEMAND_PATTERNS"
  }
  case class DemandPatterns(adorn: Set[Seq[Boolean]]) extends Hint {
    val key: Key = DemandPatterns.key
  }

  /**
   * ignore this call when collecting the inputs of the called pattern
   */
  case object IgnoreCall extends Hint {
    val key: Key = "MAGIC_IGNORE_CALL"
  }

  /**
   * use the given adornment instead of computing it
   */
  object FixedAdornment {
    val key: Key = "MAGIC_FIXED_ADORNMENT"
  }
  case class FixedAdornment(adorn: Seq[Boolean]) extends Hint {
    val key: Key = FixedAdornment.key
  }

  /**
   * do not derive input relation for this pattern or body
   */
  case object NoInputRelation extends Hint {
    val key: Key = "MAGIC_NO_INPUT_RELATION"
  }

  /**
   * indicates the adornment
   */
  object Adornment {
    val key: Key = "MAGIC_ADORNMENT"
  }
  case class Adornment(adorn: Seq[Boolean]) extends Hint {
    val key: Key = Adornment.key
  }

  object InputCall {
    val key: Key = "MAGIC_INPUT_CALL"
  }
  case class InputCall(name: String) extends Hint {
    override val key: Key = InputCall.key
  }
}
