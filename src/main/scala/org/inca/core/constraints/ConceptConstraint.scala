package org.inca.core.constraints

import org.inca.core.`type`.ICompileTimeIncAType
import org.inca.core.content.IPatternBodyContent
import org.inca.core.values.IVariableValue

case class ConceptConstraint(`var`: IVariableValue, `type`: ICompileTimeIncAType) extends IPatternBodyContent
