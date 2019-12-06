package org.inca.core_old

import org.inca.core_old.content.{IGenNameProvider, IPatternModuleContent}
import org.inca.mps.INamedConcept

trait IPatternModule extends INamedConcept with IGenNameProvider with IIncaModule {
  val contents: Seq[IPatternModuleContent]
}
