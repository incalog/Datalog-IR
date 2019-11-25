package org.inca.gp.constraints

import org.inca.core.`type`.ICompileTimeIncAType
import org.inca.core.constraints.ConceptConstraint
import org.inca.core.values.IVariableValue
import org.inca.gp.content.IGraphPatternBodyContent

case class GraphPatternConceptConstraint(override var `var`: IVariableValue,
                                         override var `type`: ICompileTimeIncAType)
  extends ConceptConstraint(`var`, `type`)
    with IGraphPatternBodyContent
