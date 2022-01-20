package inca.backend.hints

import inca.backend.hints.Hint.Key

object MagicSetHints {

  val MainKey = "MAGIC_MAIN"
  val IgnoreCallKey = "MAGIC_IGNORE_CALL"
  val FixedAdornmentKey = "MAGIC_FIXED_ADORNMENT"
  val NoInputRelationKey = "MAGIC_NO_INPUT_RELATION"
  val AdornmentKey = "MAGIC_ADORNMENT"
  val InputCallKey = "MAGIC_INPUT_CALL"

  val DemandPatternsKey = "DEMAND_DEMAND_PATTERNS"
  val InputRelationKey = "DEMAND_INPUT_RELATION"

  /**
   * marks calls from which the magic set transformation starts
   */
  object Main {
    val key: Key = MainKey
  }
  case class Main(adorn: Seq[Boolean]) extends Hint {
    val key: Key = Main.key
  }

  /**
   * indicates the demand patterns for a pattern
   */
  object DemandPatterns {
    val key: Key = DemandPatternsKey
  }
  case class DemandPatterns(adorn: Set[Seq[Boolean]]) extends Hint {
    val key: Key = DemandPatterns.key
  }

  /**
   * indicates that a relation is an input relation
   */
  case object InputRelation extends Hint {
    val key: Key = InputRelationKey
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
  object FixedAdornment {
    val key: Key = FixedAdornmentKey
  }
  case class FixedAdornment(adorn: Seq[Boolean]) extends Hint {
    val key: Key = FixedAdornment.key
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
  object Adornment {
    val key: Key = AdornmentKey
  }
  case class Adornment(adorn: Seq[Boolean]) extends Hint {
    val key: Key = Adornment.key
  }

  object InputCall {
    val key: Key = InputCallKey
  }
  case class InputCall(name: String) extends Hint {
    override val key: Key = InputCall.key
  }
}
