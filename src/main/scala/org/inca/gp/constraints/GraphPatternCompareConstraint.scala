package org.inca.gp.constraints

import org.inca.core.constraints.CompareConstraint
import org.inca.core.values.IValue
import org.inca.gp.content.IGraphPatternBodyContent

case class GraphPatternCompareConstraint(override var left: IValue,
                                         override var right: IValue)
  extends CompareConstraint(left, right)
    with IGraphPatternBodyContent
