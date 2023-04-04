package inca.backend.hints

import inca.backend.hints.Hint.Key

object MagicSetHints {

  val MainKey = "MAGIC_MAIN"
  val IgnoreCallKey = "MAGIC_IGNORE_CALL"
  val FixedAdornmentKey = "MAGIC_FIXED_ADORNMENT"
  val NoInputRelationKey = "MAGIC_NO_INPUT_RELATION"
  val AdornmentsKey = "MAGIC_ADORNMENTS"
  val InputCallKey = "MAGIC_INPUT_CALL"

  val DemandPatternsKey = "DEMAND_DEMAND_PATTERNS"
  val InputRelationKey = "DEMAND_INPUT_RELATION"

  /**
   * marks calls from which the magic set transformation starts
   */
  case class Main(adorn: Seq[Boolean]) extends Hint {
    val key: Key = MainKey
  }

  /**
   * indicates the demand patterns for a pattern
   */
  case class DemandPatterns(adorn: Set[Seq[Boolean]]) extends Hint {
    val key: Key = DemandPatternsKey
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
  case class Adornments(adorn: Set[Seq[Boolean]]) extends Hint {
    val key: Key = AdornmentsKey
  }
  object Adornments {
    def singleton(adorn: Seq[Boolean]): Adornments = Adornments(Set(adorn))
  }

  case class InputCall(name: String) extends Hint {
    override def key: Key = InputCallKey
  }
}
