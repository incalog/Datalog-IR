package inca.frontend.constraint.extensions.boolOps

import inca.frontend.constraint.core.tree._

object Trees {
  case class Not(cond: Expression) extends Expression {
    override def freeVars: Map[Name, Option[Type]] = cond.freeVars
    override def prettyprint(implicit indent: String): String = s"!(${cond.prettyprint})"
  }
  case class And(e1: Expression, e2: Expression) extends Expression {
    override def freeVars: Map[Name, Option[Type]] = e1.freeVars ++ e2.freeVars
    override def prettyprint(implicit indent: String): String = s"(${e1.prettyprint} && ${e2.prettyprint})"
  }
  case class Or(e1: Expression, e2: Expression) extends Expression {
    override def freeVars: Map[Name, Option[Type]] = e1.freeVars ++ e2.freeVars
    override def prettyprint(implicit indent: String): String = s"(${e1.prettyprint} || ${e2.prettyprint})"
  }
}
