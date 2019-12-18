package org.inca.lang.core

import org.inca.lang.core.Content.{IGenNameProvider, IVariable}
import org.inca.lang.core.Values.IVariableValue

// todo maybe refactor if no further concepts are added from mps inca -> `util.scala` file?
object Reference {
  case class VariableReference(variable: IVariable) extends IGenNameProvider with IVariableValue
}
