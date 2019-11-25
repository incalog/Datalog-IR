package org.inca.core.constraints

import org.inca.core.content.IPatternBodyContent
import org.inca.core.misc.{IContainsJavaExpression, IJavaContext}
import org.inca.mps.binaryOperations.Expression

case class CheckConstraint(expression: Expression)
  extends IPatternBodyContent
    with IJavaContext
    with IContainsJavaExpression
