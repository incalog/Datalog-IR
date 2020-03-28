package org.inca.lang.fun

import org.inca.lang.core.Content.{PatternBody, PatternBodyContent}
import org.inca.lang.core.Misc.ITransformable
import org.inca.lang.core._

trait IStatement extends PatternBodyContent with ITypeConstraintProvider with ITransformable

case class ReturnStatement(expression: IExpression) extends IStatement with ITypeConstraintProvider
case class AssignStatement(override val left: IExpression,
                           override val right: IExpression) extends AbstractBinary with IStatement
case class AssertStatement(condition: ICondition) extends IStatement
case class StatementList(override val contents: Seq[PatternBodyContent])
  extends IStatement with PatternBody with IReturnContainer
case class SwitchStatement(alternatives: Seq[PatternFunctionBody])
  extends IStatement with IReturnContainer
case class SwitchStatementNoOptimizationAttribute()