package org.inca.core_old.content

import org.inca.core_old.typ.ICompileTimeIncAType
import org.inca.mps.INamedConcept

trait IJoinTypeDef extends INamedConcept {
  var types: List[ICompileTimeIncAType]
}
