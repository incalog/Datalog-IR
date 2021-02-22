package inca.backend.ir

import inca.runtime.Query
import inca.runtime.context.LanguageMetaInfo

object PSystem {

  /** The GP compiler generates instances of PSystem.Module. */
  trait Module {
    val patterns: Map[String, () => Query.Specification]
    val lmi: LanguageMetaInfo
  }

}
