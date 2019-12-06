package org.inca.gp_old.constraints

import org.inca.core_old.constraints.CompareConstraint
import org.inca.core_old.values.IValue
import org.inca.gp_old.content.IGraphPatternBodyContent

case class GraphPatternCompareConstraint(override var left: IValue,
                                         override var right: IValue)
  extends CompareConstraint(left, right)
    with IGraphPatternBodyContent
