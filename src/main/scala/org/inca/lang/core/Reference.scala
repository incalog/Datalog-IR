package org.inca.lang.core

import org.inca.lang.core.Content.{IGenNameProvider, IVariable}
import org.inca.lang.core.Values.IVariableValue

// todo maybe refactor if no further concepts are added from mps inca -> `util.scala` file?
object Reference {
  abstract class VariableReference(variable: IVariable)
  case class CoreVariableReference(variable: IVariable)
    extends VariableReference(variable) with IGenNameProvider with IVariableValue
}
