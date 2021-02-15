package inca.frontend_old.extensions.foreach

import inca.frontend_old.core
import inca.frontend_old.core.tree
import inca.frontend_old.core.tree._

trait Trees extends core.Trees with Syntax {
  override def Foreach(name: tree.Name, exp: Expression, body: Body): Statement = Trees.Foreach(name, exp, body)
}

object Trees {
  case class Foreach(name: Name, exp: Expression, body: Body) extends Statement with Var.Target {
    val elemTyp: Option[Type] = exp.typ.flatMap {
      case ty: TIterable => Some(ty.contained)
      case _ => None
    }

    override def boundVars: Set[Name] = Set(name) ++ body.boundVars
    override def allVars: Map[Name, Option[Type]] = Map(name -> elemTyp) ++ exp.freeVars ++ body.allVars

    override def prettyprint(implicit indent: String): String = {
      s"${indent}foreach $name in ${exp.prettyprint} ${body.prettyprint}"
    }
  }

}
