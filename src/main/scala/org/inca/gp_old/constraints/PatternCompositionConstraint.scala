package org.inca.gp_old.constraints

import org.inca.core.ITypeConstraintProvider
import org.inca.core.constraints.IPatternCall
import org.inca.gp_old.content.IGraphPatternBodyContent

case class PatternCompositionConstraint(neg: Boolean,
                                        call: IPatternCall)
  extends IGraphPatternBodyContent
    with ITypeConstraintProvider
