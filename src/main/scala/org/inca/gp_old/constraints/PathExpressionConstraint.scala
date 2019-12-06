package org.inca.gp_old.constraints

import org.inca.core_old.ITypeConstraintProvider
import org.inca.core_old.typ.ICompileTimeIncAType
import org.inca.core_old.constraints.{IPathElement, IPathExpressionLike}
import org.inca.core_old.reference.VariableReference
import org.inca.core_old.values.IValue
import org.inca.gp_old.content.IGraphPatternBodyContent

case class PathExpressionConstraint(src: VariableReference,
                                    trg: IValue,
                                    element: IPathElement,
                                    `type`: ICompileTimeIncAType)
  extends ITypeConstraintProvider
    with IGraphPatternBodyContent
    with IPathExpressionLike