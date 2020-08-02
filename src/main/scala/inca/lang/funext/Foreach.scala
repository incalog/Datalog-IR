package inca.lang.funext

import inca.lang.fun.Fun._
import inca.lang.funext.desugar.{DesugarTrans, Desugarable}
import inca.util.Gensym
import inca.util.Meta.TAB

case class Foreach(name: Name, exp: Exp, body: Seq[Statement]) extends Statement {
  override def usedvars: Set[Name] = Set(name) ++ exp.usedvars ++ body.flatMap(_.usedvars)

  override def prettyprint(implicit indent: String): String = {
    val bodyS = if (body.isEmpty) "" else
      "\n" + body.map(_.prettyprint(indent+TAB)).mkString("\n")
    s"""${indent}for $name in ${exp.prettyprint} {$bodyS
       |${indent}}""".stripMargin
  }
}

object Foreach extends Desugarable {
  override def trans(): DesugarTrans = new DesugarTrans {
    override def desugarStm(stm: Statement)(implicit gensym: Gensym): Seq[Statement] = stm match {
      case Foreach(name, exp, body) => exp.typ match {
        case Some(TList(ty)) => changed(Assign(Seq(name), PathAccess(exp, ChildrenLink).typed(ty)) +: body.flatMap(desugarStm))
        case Some(TEnumeration(_)) => changed(Assign(Seq(name), exp) +: body.flatMap(desugarStm))
        case Some(ty) => throw new IllegalArgumentException(s"Foreach loop expression $exp must have iterable type, but was type $ty")
        case None => throw new IllegalArgumentException(s"Cannot support foreach loop with untyped expression $exp")
      }

      case _ => super.desugarStm(stm)
    }
  }
}
