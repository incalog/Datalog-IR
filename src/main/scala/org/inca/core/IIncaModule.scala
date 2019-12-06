package org.inca.core

import org.inca.mps.INamedConcept

trait IIncaModule extends INamedConcept {
  val imports: Seq[IIncaModuleImport]
}
