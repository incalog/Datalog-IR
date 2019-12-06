package org.inca.core

import org.inca.core.Content.{IGenNameProvider, IVariable}
import org.inca.core.Values.IVariableValue

// todo maybe refactor if no further concepts are added from mps inca
object Reference {
  case class VariableReference(variable: IVariable) extends IGenNameProvider with IVariableValue
}
