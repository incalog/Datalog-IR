package inca.frontend.extensions

import inca.frontend.core.Core._
import inca.frontend.desugar.{DesugarTrans, Desugarable}
import inca.util.Gensym

import scala.collection.mutable.ListBuffer

case class Cast(src: Exp, targetTyp: TypeAnno) extends Exp {
  override def freeVars: Map[Name, Option[TypeAnno]] = src.freeVars

  override def prettyprint(implicit indent: String): String =
    s"${src.prettyprint}:${targetTyp.prettyprint}"
}

object Cast extends Desugarable with Function2[Exp, TypeAnno, Exp] {
  override def trans(): DesugarTrans = new DesugarTrans {
    val castStatements: ListBuffer[Statement] = ListBuffer()

    override def desugarExp(exp: Exp)(implicit gensym: Gensym): Exp = exp match {
      case Cast(src, targetTyp) =>
        val desugaredSrc = desugarExp(src)
        val sym = desugaredSrc match {
          case Var(v) =>
            v
          case _ =>
            val v = gensym.fresh("cast")
            castStatements += Assign(Seq(v), desugaredSrc)
            v
        }
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
        castStatements.clear()
        prepend ++ desugared
      }
    }
  }
}
