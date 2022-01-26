package inca.backend.hints
import inca.backend.hints.Hint.Key

object OptimizationHints {
  object KeepPattern extends Hint {
    override val key: Key = "KEEP_PATTERN"
  }
}
