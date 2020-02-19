package org.inca.lang.fun

import org.inca.lang.core.Constraints.{IPathElement, IPathExpressionLike, IPatternCall}
import org.inca.lang.core.Content.{CoreTemporaryVariable, IVariable, TemporaryVariable}
import org.inca.lang.core.ITypeConstraintProvider
import org.inca.lang.core.Reference.{CoreVariableReference, VariableReference}
import org.inca.lang.core.Typ.ITypeHintConsumer
import org.inca.lang.core.Values.{AbstractLiteralValue, ExpressionEvaluationValue, IValue}
import org.inca.meta.MetaElements.MetaElement

trait IExpression extends IStatement with ICondition

trait ITuple extends IExpression


case class EvalValue(expression: ExpressionEvaluationValue) extends IExpression

case class FunTemporaryVariable(name: String,
                                typ: Option[MetaElement])
  extends TemporaryVariable(name, typ) with ITuple

case class FunVariableReference(variable: IVariable)
  extends VariableReference(variable) with ITuple with ITypeHintConsumer

case class LiteralValue(value: AbstractLiteralValue) extends IExpression with IValue

case class PathExpression(src: IExpression, element: IPathElement)
  extends IExpression with IValue with IPathExpressionLike

case class PatternFunctionCall(call: IPatternCall)
  extends IExpression with IStatement with ITypeConstraintProvider

case class Tuple(expression: Seq[IExpression]) extends ITuple