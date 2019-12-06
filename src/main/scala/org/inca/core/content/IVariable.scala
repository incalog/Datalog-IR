package org.inca.core.content

import org.inca.core.typ.ICompileTimeIncAType
import org.inca.mps.INamedConcept

trait IVariable extends INamedConcept with IGenNameProvider {
  val typ: Option[ICompileTimeIncAType]
}