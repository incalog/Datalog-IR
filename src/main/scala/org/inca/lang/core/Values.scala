package org.inca.lang.core

import org.inca.lang.core.Content.IGenNameProvider
import org.inca.lang.core.Misc.{IContainsJavaExpression, IJavaContext}

object Values {
  trait IValue extends IGenNameProvider
  trait IVariableValue extends IGenNameProvider with IValue

  abstract class AbstractLiteralValue extends IValue
  abstract class ComputationValue extends IValue
  case class BoolValue(value: Boolean) extends AbstractLiteralValue
  // todo eval func should be ` => Boolean` was `expression: Expression`
  case class ExpressionEvaluationValue(const: Boolean,
                                       unwind: Boolean,
                                       evalFunc:  Boolean)
    extends ComputationValue with IJavaContext with IContainsJavaExpression
}
