package org.inca.lang.core

import org.inca.lang.core.Content.GenNameProvider
import org.inca.lang.core.Misc.{IContainsJavaExpression, IJavaContext}

object Values {

  trait Value extends GenNameProvider

  trait VariableValue extends GenNameProvider with Value

  abstract class AbstractLiteralValue extends Value

  abstract class ComputationValue extends Value

  case class BoolValue(value: Boolean) extends AbstractLiteralValue

  // todo eval func should be ` => Boolean` was `expression: Expression`
  case class ExpressionEvaluationValue(const: Boolean,
                                       unwind: Boolean,
                                       evalFunc: Boolean)
    extends ComputationValue with IJavaContext with IContainsJavaExpression

}
