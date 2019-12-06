package org.inca.core_old.content

import org.inca.core_old.typ.ICompileTimeIncAType
import org.inca.mps.INamedConcept

trait IVariable extends INamedConcept with IGenNameProvider {
  val typ: Option[ICompileTimeIncAType]
}