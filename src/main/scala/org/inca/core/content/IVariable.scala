package org.inca.core.content

import org.inca.core.`type`.ICompileTimeIncAType
import org.inca.mps.INamedConcept

trait IVariable extends INamedConcept with IGenNameProvider {
  var `type`: ICompileTimeIncAType
}