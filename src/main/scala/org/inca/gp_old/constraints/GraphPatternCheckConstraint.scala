package org.inca.gp_old.constraints

import org.inca.core_old.constraints.CheckConstraint
import org.inca.gp_old.content.IGraphPatternBodyContent
import org.inca.mps.binaryOperations.Expression

case class GraphPatternCheckConstraint(override var expression: Expression)
  extends CheckConstraint(expression)
    with IGraphPatternBodyContent

