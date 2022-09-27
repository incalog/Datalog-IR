package inca.backend.hints

import inca.backend.hints.Hint.Key

object ObjectHints {

  val AllocationKey = "OBJECT_ALLOCATION"
  val AllocationRootKey = "OBJECT_ALLOCATION_ROOT"

  val TimestampKey = "OBJECT_TIMESTAMP"
  val TimestampRootKey = "OBJECT_TIMESTAMP_ROOT"

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
   * The root pattern for all timestamp propagations. It initializes the timestamp.
   */
  object TimestampRoot extends Hint {
    override def key: Key = TimestampRootKey
  }

  /**
   * Pattern requires a timestamp to be functional.
   */
  object RequiresTimestamp extends Hint {
    override def key: Key = TimestampKey
  }
}