package org.inca.lang.fun

import org.inca.lang.Core._
import org.inca.lang.Gp._
import org.inca.meta.MetaElements.MetaElement

trait IExpression extends IStatement with ICondition

trait ITuple extends IExpression



case class FunTemporaryVariable(name: String,
                                typ: Option[MetaElement])
  extends AbstractTemporaryVariable(name, typ) with ITuple

case class FunVariableReference(variable: Variable)
  extends AbstractVariableReference(variable) with ITuple

//case class LiteralValue(value: AbstractLiteralValue) extends IExpression with Value

case class PathExpression(src: IExpression, element: PathElement)
  extends IExpression with Value

case class PatternFunctionCall(call: PatternCall)
  extends IExpression with IStatement

case class Tuple(expression: Seq[IExpression]) extends ITuple