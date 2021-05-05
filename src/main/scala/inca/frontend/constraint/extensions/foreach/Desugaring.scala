package inca.frontend.constraint.extensions.foreach

import inca.frontend.constraint.core.tree._
import inca.frontend.constraint.desugar.{DesugarTrans, Desugarable}
import inca.frontend.constraint.extensions.foreach.Trees._
import inca.util.Gensym

object Desugaring extends Desugarable {
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