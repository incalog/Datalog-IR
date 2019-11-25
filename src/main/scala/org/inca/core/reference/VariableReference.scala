package org.inca.core.reference

import org.inca.core.`type`.ICompileTimeIncAType
import org.inca.core.content.{IGenNameProvider, IVariable}
import org.inca.core.values.IVariableValue

case class VariableReference(variable: IVariable) extends IGenNameProvider with IVariableValue
