package org.inca.core_old

import org.inca.mps.INamedConcept

trait IIncaModule extends INamedConcept {
  val imports: Seq[IIncaModuleImport]
}
