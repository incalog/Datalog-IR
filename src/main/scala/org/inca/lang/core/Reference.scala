package org.inca.lang.core

import org.inca.lang.core.Content.{GenNameProvider, Variable}
import org.inca.lang.core.Values.VariableValue

// todo maybe refactor if no further concepts are added from mps inca -> `util.scala` file?
object Reference {
  abstract class VariableReference(variable: Variable)
  case class CoreVariableReference(variable: Variable)
    extends VariableReference(variable) with GenNameProvider with VariableValue
}
