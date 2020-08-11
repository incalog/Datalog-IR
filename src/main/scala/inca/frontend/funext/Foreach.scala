package inca.frontend.funext

import inca.frontend.desugar.{DesugarTrans, Desugarable}
import inca.frontend.fun.Fun._
import inca.util.Gensym

case class Foreach(name: Name, exp: Exp, body: Body) extends Statement {
  val elemTyp: Option[TLinked] = exp.typ.flatMap {
    case ty: TIterable => Some(ty.contained)
    case _ => None
  }

  override def boundVars: Set[Name] = Set(name) ++ body.boundVars
  override def allVars: Map[Name, Option[TypeAnno]] = Map(name -> elemTyp) ++ exp.freeVars ++ body.allVars

  override def prettyprint(implicit indent: String): String = {
    s"${indent}foreach $name in ${exp.prettyprint} ${body.prettyprint}"
  }
}

object Foreach extends Desugarable {
  override def trans(): DesugarTrans = new DesugarTrans {
    override def desugarStm(stm: Statement)(implicit gensym: Gensym): Seq[Statement] = stm match {
      case Foreach(name, exp, body) => exp.typ match {
        case Some(TList(ty)) => changed(Assign(Seq(name), PathAccess(exp, ChildrenLink).typed(ty)) +: body.stmts.flatMap(desugarStm))
        case Some(TEnumeration(ty)) => changed(Assign(Seq(name), exp.orTyped(ty)) +: body.stmts.flatMap(desugarStm))
        case Some(ty) => throw new IllegalArgumentException(s"Foreach loop expression $exp must have iterable type, but was type $ty")
        case None => throw new IllegalArgumentException(s"Cannot support foreach loop with untyped expression $exp")
      }

      case _ => super.desugarStm(stm)
    }
  }
}
