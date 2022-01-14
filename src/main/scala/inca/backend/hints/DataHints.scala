package inca.backend.hints
import inca.backend.hints.Hint.Key

object DataHints {

  /**
   * The annotated pattern represents a constructor.
   */
  object Constructor extends Hint {
    override val key: Key = "DATA_CONSTRUCTOR"
  }

  /**
   * The annotated pattern represents a selector.
   */
  object Selector extends Hint {
    override val key: Key = "DATA_SELECTOR"
  }

  /**
   * The annotated pattern represents a data type.
   */
  object DataType extends Hint {
    override val key: Key = "DATA_TYPE"
  }

  /**
   * The annotated body represents a rule to construct a value during run time
   */
  object IDBConstructor extends Hint {
    override val key: Key = "IDB_DATA_CONSTRUCTOR"
  }
}
