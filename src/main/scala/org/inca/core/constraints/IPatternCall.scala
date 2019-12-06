package org.inca.core.constraints

import org.inca.core.content.IPattern
import org.inca.core.values.IValue

trait IPatternCall {
  val transitive: Boolean
  val arguments: Seq[IValue]
  val pattern: IPattern
}
