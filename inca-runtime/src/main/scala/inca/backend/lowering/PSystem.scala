package inca.backend.lowering

import inca.runtime.Query

object PSystem {

  /** The GP compiler generates instances of PSystem.Module. */
  trait Module {
    val patterns: Map[String, () => Query.Specification]
  }

}
