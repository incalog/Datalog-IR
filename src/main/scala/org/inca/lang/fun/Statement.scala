package org.inca.lang.fun

import org.inca.lang.core.Content.{IPatternBody, IPatternBodyContent}
import org.inca.lang.core.Misc.ITransformable
import org.inca.lang.core._

trait IStatement extends IPatternBodyContent with ITypeConstraintProvider with ITransformable

case class ReturnStatement(expression: IExpression) extends IStatement with ITypeConstraintProvider
case class AssignStatement(override val left: IExpression,
                           override val right: IExpression) extends AbstractBinary with IStatement
case class AssertStatement(condition: ICondition) extends IStatement
case class StatementList(override val contents: Seq[IPatternBodyContent])
  extends IStatement with IPatternBody with IReturnContainer
case class SwitchStatement(alternatives: Seq[PatternFunctionBody])
  extends IStatement with IReturnContainer
case class SwitchStatementNoOptimizationAttribute()