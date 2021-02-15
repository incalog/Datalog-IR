package inca.frontend_old.extensions.forallExists

import inca.frontend_old.core
import inca.frontend_old.core.tree._

trait Trees extends core.Trees with Syntax {
  override def Forall(name: Name, exp: Expression, body: Body): Statement = Trees.Forall(name, exp, body)
  override def Exists(name: Name, exp: Expression, body: Body): Statement = Trees.Exists(name, exp, body)
}

object Trees {
  case class Forall(name: Name, exp: Expression, body: Body) extends Statement with Var.Target {
    val elemTyp: Option[Type] = exp.typ.flatMap {
      case ty: TIterable => Some(ty.contained)
      case _ => None
    }

    override def boundVars: Set[Name] = Set(name) ++ body.boundVars
    override def allVars: Map[Name, Option[Type]] = Map(name -> elemTyp) ++ exp.freeVars ++ body.allVars

    override def prettyprint(implicit indent: String): String =
      s"${indent}forall $name in ${exp.prettyprint} ${body.prettyprint}"
  }

  case class Exists(name: Name, exp: Expression, body: Body) extends Statement with Var.Target {
    val elemTyp: Option[Type] = exp.typ.flatMap {
      case ty: TIterable => Some(ty.contained)
      case _ => None
    }

    override def boundVars: Set[Name] = Set(name) ++ body.boundVars
    override def allVars: Map[Name, Option[Type]] = Map(name -> elemTyp) ++ exp.freeVars ++ body.allVars

    override def prettyprint(implicit indent: String): String =
      s"${indent}exists $name in ${exp.prettyprint} ${body.prettyprint}"
  }
}
