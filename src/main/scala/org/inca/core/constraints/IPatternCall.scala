package org.inca.core.constraints

import org.inca.core.content.IPattern
import org.inca.core.values.IValue

trait IPatternCall {
  var transitive: Boolean
  var arguments: List[IValue]
  var pattern: IPattern
}
