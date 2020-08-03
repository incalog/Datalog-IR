package inca.lang.funext

import inca.lang.fun.Fun._
import inca.lang.funext.desugar.{DesugarTrans, Desugarable}
import inca.util.Gensym

import scala.collection.mutable.ListBuffer

/** Enumerates the values of the given type */
case class Enum(ty: TypeAnno) extends Exp {
  override def usedvars: Map[Name, Option[TypeAnno]] = Map()
  override def prettyprint(implicit indent: String): String = s"enum(${ty.prettyprint})"
}

object Enum extends Desugarable {

  override def trans(): DesugarTrans = new DesugarTrans {
    var enumStatements: ListBuffer[Statement] = ListBuffer()

    override def desugarExp(exp: Exp)(implicit gensym: Gensym): Exp = exp match {
      case Enum(ty) =>
        val sym = gensym.fresh(s"enum_${ty.javastring}")
        enumStatements += Assert(InstanceOf(Var(sym), ty))
        changed(Var(sym))
      case _ => super.desugarExp(exp)
    }

    override def desugarStm(stm: Statement)(implicit gensym: Gensym): Seq[Statement] = {
      val desugared = super.desugarStm(stm)
      if (enumStatements.isEmpty)
        desugared
      else {
        val prepend = enumStatements.toSeq
        enumStatements = ListBuffer()
        prepend ++ desugared
      }
    }
  }
}
