package org.inca.lang.fun

import org.inca.meta.MetaElements.MetaElement

trait ICondition

trait IInstanceOfLike {
  val typ: MetaElement
  val expression: IExpression
}

case class Def(expression: IExpression) extends ICondition
case class Undef(expression: IExpression) extends ICondition

case class Equality(left: IExpression, right: IExpression)
  extends AbstractBinary with ICondition

case class Inequality(left: IExpression, right: IExpression)
  extends AbstractBinary with ICondition

case class InstanceOf(typ: MetaElement,
                      expression: IExpression)
  extends ICondition with IInstanceOfLike

case class NotInstanceOf(typ: MetaElement,
                      expression: IExpression)
  extends ICondition with IInstanceOfLike