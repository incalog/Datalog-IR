package org.inca.core_old.constraints

import org.inca.core_old.ITypeConstraintProvider
import org.inca.core_old.typ.ICompileTimeIncAType
import org.inca.core_old.content.IPatternBodyContent
import org.inca.core_old.values.IVariableValue

abstract class ConceptConstraint(vari: IVariableValue, typ: ICompileTimeIncAType)
  extends IPatternBodyContent with ITypeConstraintProvider
