package org.inca.gp.constraints

import org.inca.core.ITypeConstraintProvider
import org.inca.core.`type`.ICompileTimeIncAType
import org.inca.core.constraints.{IPathElement, IPathExpressionLike}
import org.inca.core.reference.VariableReference
import org.inca.core.values.IValue
import org.inca.gp.content.IGraphPatternBodyContent

case class PathExpressionConstraint(src: VariableReference,
                                    trg: IValue,
                                    element: IPathElement,
                                    `type`: ICompileTimeIncAType)
  extends ITypeConstraintProvider
    with IGraphPatternBodyContent
    with IPathExpressionLike