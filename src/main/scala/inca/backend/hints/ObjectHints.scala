package inca.backend.hints

import inca.backend.hints.Hint.Key

object ObjectHints {

  val AllocationKey = "OBJECT_ALLOCATION"
  val AllocationRootKey = "OBJECT_ALLOCATION_ROOT"

  /**
   * The annotated pattern represents an object constructor
   */
  object Allocation extends Hint {
    override def key: Key = AllocationKey
  }

  object AllocationRoot extends Hint {
    override def key: Key = AllocationRootKey
  }
}