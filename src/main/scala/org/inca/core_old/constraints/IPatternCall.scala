package org.inca.core_old.constraints

import org.inca.core_old.content.IPattern
import org.inca.core_old.values.IValue

trait IPatternCall {
  val transitive: Boolean
  val arguments: Seq[IValue]
  val pattern: IPattern
}
