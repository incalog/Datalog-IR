package inca.lang.funext

import inca.lang.fun.Fun._
import inca.lang.funext.desugar.{DesugarTrans, Desugarable}
import inca.util.Gensym

import scala.collection.mutable.ListBuffer

case class Cast(src: Exp, targetTyp: TypeAnno) extends Exp {
  override def usedvars: Map[Name, Option[TypeAnno]] = src.usedvars

  override def prettyprint(implicit indent: String): String =
    s"${src.prettyprint}:${targetTyp.prettyprint}"
}

object Cast extends Desugarable {
  override def trans(): DesugarTrans = new DesugarTrans {
    var castStatements: ListBuffer[Statement] = ListBuffer()

    override def desugarExp(exp: Exp)(implicit gensym: Gensym): Exp = exp match {
      case Cast(src, targetTyp) =>
        val desugaredSrc = desugarExp(src)
        val sym = gensym.fresh("cast")
        castStatements += Assign(Seq(sym), desugaredSrc)
        castStatements += Assert(InstanceOf(Var(sym), targetTyp))
        changed(Var(sym).typed(targetTyp))

      case _ => super.desugarExp(exp)
    }

    override def desugarStm(stm: Statement)(implicit gensym: Gensym): Seq[Statement] = {
      val desugared = super.desugarStm(stm)
      if (castStatements.isEmpty)
        desugared
      else {
        val prepend = castStatements.toSeq
        castStatements = ListBuffer()
        prepend ++ desugared
      }
    }
  }
}
