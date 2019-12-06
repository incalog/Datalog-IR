package org.inca.core.content

import org.inca.core.typ.ICompileTimeIncAType
import org.inca.core.values.IVariableValue

case class TemporaryVariable(name: String, typ: Option[ICompileTimeIncAType]) extends IVariable with IVariableValue
