package inca.backend.hints
import inca.backend.hints.Hint.Key

object DataHints {

  val ConstructorKey = "DATA_CONSTRUCTOR"
  val SelectorKey = "DATA_SELECTOR"

  /**
   * The annotated pattern represents a constructor.
   */
  object Constructor extends Hint {
    override def key: Key = ConstructorKey
  }

  /**
   * The annotated pattern represents a selector.
   */
  object Selector extends Hint {
    override def key: Key = SelectorKey
  }
}
