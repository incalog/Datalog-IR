package inca.frontend.extensions.boolOps

import inca.frontend.core
import inca.frontend.core.tree._

trait Trees extends core.Trees with Syntax {
  override def Not(cond: Expression): Expression = Trees.Not(cond)
  override def Or(e1: Expression, e2: Expression): Expression = Trees.Or(e1, e2)
  override def And(e1: Expression, e2: Expression): Expression = Trees.And(e1, e2)
}

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
