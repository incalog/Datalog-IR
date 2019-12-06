package org.inca.core_old.reference

import org.inca.core_old.typ.ICompileTimeIncAType
import org.inca.core_old.content.{IGenNameProvider, IVariable}
import org.inca.core_old.values.IVariableValue

case class VariableReference(variable: IVariable) extends IGenNameProvider with IVariableValue
