package org.inca.core.constraints

import org.inca.core.ITypeConstraintProvider
import org.inca.core.typ.ICompileTimeIncAType
import org.inca.core.content.IPatternBodyContent
import org.inca.core.values.IVariableValue

abstract class ConceptConstraint(vari: IVariableValue, typ: ICompileTimeIncAType)
  extends IPatternBodyContent with ITypeConstraintProvider
