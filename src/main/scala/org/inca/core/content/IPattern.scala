package org.inca.core.content

import org.inca.mps.INamedConcept

trait IPattern extends INamedConcept with IPatternModuleContent with IGenNameProvider with IVariableBinder {
  val parameters: Seq[IParameter]
  val bodies: Seq[IPatternBody]
  val visibility: Option[IPatternVisibility]
}
