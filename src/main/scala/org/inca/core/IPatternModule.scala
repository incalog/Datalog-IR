package org.inca.core

import org.inca.core.content.{IGenNameProvider, IPatternModuleContent}
import org.inca.mps.INamedConcept

trait IPatternModule extends INamedConcept with IGenNameProvider with IIncaModule {
  val contents: Seq[IPatternModuleContent]
}
