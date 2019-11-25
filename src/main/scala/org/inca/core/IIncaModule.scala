package org.inca.core

import org.inca.mps.INamedConcept

trait IIncaModule extends INamedConcept {
  var imports: List[IIncaModuleImport]
}
