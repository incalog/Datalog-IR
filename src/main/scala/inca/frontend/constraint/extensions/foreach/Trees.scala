package inca.frontend.constraint.extensions.foreach

import inca.frontend.constraint.core.tree._

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
