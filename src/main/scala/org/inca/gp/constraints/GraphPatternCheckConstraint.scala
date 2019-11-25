package org.inca.gp.constraints

import org.inca.core.constraints.CheckConstraint
import org.inca.gp.content.IGraphPatternBodyContent
import org.inca.mps.binaryOperations.Expression

case class GraphPatternCheckConstraint(override var expression: Expression)
  extends CheckConstraint(expression)
    with IGraphPatternBodyContent

