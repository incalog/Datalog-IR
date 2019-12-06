package org.inca.core.content

import org.inca.core.typ.ICompileTimeIncAType
import org.inca.mps.INamedConcept

trait IJoinTypeDef extends INamedConcept {
  var types: List[ICompileTimeIncAType]
}
