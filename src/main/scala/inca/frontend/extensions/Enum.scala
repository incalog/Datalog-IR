package inca.frontend.extensions

import inca.frontend.core.Core._
import inca.frontend.desugar.{DesugarTrans, Desugarable}
import inca.util.Gensym

import scala.collection.mutable.ListBuffer

/** Enumerates the values of the given type */
case class Enum(ty: TypeAnno) extends Exp {
  override def freeVars: Map[Name, Option[TypeAnno]] = Map()
  override def prettyprint(implicit indent: String): String = s"enum(${ty.prettyprint})"
}

object Enum extends Desugarable {

  override def trans(): DesugarTrans = new DesugarTrans {
    val enumStatements: ListBuffer[Statement] = ListBuffer()

    override def desugarExp(exp: Exp)(implicit gensym: Gensym): Exp = exp match {
      case Enum(ty) =>
        val sym = gensym.fresh(s"enum_${ty.javastring}")
        enumStatements += Values(sym, ty)
        changed(Var(sym).typed(ty))
      case _ => super.desugarExp(exp)
    }

    override def desugarStm(stm: Statement)(implicit gensym: Gensym): Seq[Statement] = {
      val desugared = super.desugarStm(stm)
      if (enumStatements.isEmpty)
        desugared
      else {
        val prepend = enumStatements.toSeq
        enumStatements.clear()
        prepend ++ desugared
      }
    }
  }
}
