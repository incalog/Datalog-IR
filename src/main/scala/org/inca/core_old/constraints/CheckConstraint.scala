package org.inca.core_old.constraints

import org.inca.core_old.content.IPatternBodyContent
import org.inca.core_old.misc.{IContainsJavaExpression, IJavaContext}
import org.inca.mps.binaryOperations.Expression

// todo `expression` mps removal
abstract class CheckConstraint(expression: Expression)
  extends IPatternBodyContent
    with IJavaContext
    with IContainsJavaExpression
