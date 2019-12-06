package org.inca.gp_old.constraints

import org.inca.core.typ.ICompileTimeIncAType
import org.inca.core.constraints.ConceptConstraint
import org.inca.core.values.IVariableValue
import org.inca.gp_old.content.IGraphPatternBodyContent

case class GraphPatternConceptConstraint(override var vari: IVariableValue,
                                         override var typ: ICompileTimeIncAType)
  extends ConceptConstraint(vari, typ)
    with IGraphPatternBodyContent
