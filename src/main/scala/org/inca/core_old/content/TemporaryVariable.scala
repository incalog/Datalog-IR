package org.inca.core_old.content

import org.inca.core_old.typ.ICompileTimeIncAType
import org.inca.core_old.values.IVariableValue

case class TemporaryVariable(name: String, typ: Option[ICompileTimeIncAType]) extends IVariable with IVariableValue
