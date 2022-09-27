package inca.backend.hints

import inca.backend.hints.Hint.Key

object ObjectHints {

  val AllocationKey = "OBJECT_ALLOCATION"
  val AllocationRootKey = "OBJECT_ALLOCATION_ROOT"

  val FieldKey = "OBJECT_FIELD"
  val FieldGetKey = "OBJECT_FIELD_GET"
  val FieldSetKey = "OBJECT_FIELD_SET"
  val FieldRootKey = "OBJECT_FIELD_ROOT"

  /**
   * The annotated pattern represents an object constructor
   */
  object Allocation extends Hint {
    override def key: Key = AllocationKey
  }

  /**
   * The root pattern for all allocation propagations. It initializes the allocation counter.
   */
  object AllocationRoot extends Hint {
    override def key: Key = AllocationRootKey
  }

  /**
   * The root for all field pattern . It initializes the timestamp.
   */
  object FieldRoot extends Hint {
    override def key: Key = FieldRootKey
  }

  /**
   * Pattern stores the values of a field.
   */
  object Field extends Hint {
    override def key: Key = FieldKey
  }

  /**
   * Call reads the value of a field.
   */
  object FieldGet extends Hint {
    override def key: Key = FieldGetKey
  }

  /**
   * Call sets the value of a field.
   */
  object FieldSet extends Hint {
    override def key: Key = FieldSetKey
  }
}