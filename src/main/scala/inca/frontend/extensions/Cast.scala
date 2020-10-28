package inca.frontend.extensions

import inca.frontend.Frontend
import inca.frontend.core.Core._
import inca.frontend.desugar.{DesugarTrans, Desugarable}
import inca.frontend.typechecker.TypeOps
import inca.util.Gensym

import scala.collection.mutable.ListBuffer

case class Cast(src: Exp, targetTyp: TypeAnno) extends Exp {
  override def freeVars: Map[Name, Option[TypeAnno]] = src.freeVars

  override def prettyprint(implicit indent: String): String =
    s"${src.prettyprint}:${targetTyp.prettyprint}"
}

/**
 * Extension adding cast expressions to @see Parser.
 */
trait CastFrontend extends Frontend {
  import fastparse.ScalaWhitespace._
  import fastparse._

  override protected def desugarables: Seq[Desugarable] = Cast +: super.desugarables

  override protected[frontend] def trailExp[_: P]: P[Exp => Exp] =
    P(":" ~ typeAnno).mapWithLocFun[Exp, Exp](ty => Cast(_, ty)) |
      super.trailExp

  override def typecheckInternal(exp: Exp, anno: Option[TypeAnno]): TypeAnno = exp match {
    case Cast(src, targetTyp) =>
      val ety = typecheck(src)
      if (TypeOps.meet(ety, targetTyp, lang) == TNothing) {
        warn(s"Cast of type $ety to unrelated type $targetTyp will always fail", exp)
      }
      targetTyp
    case _ => super.typecheckInternal(exp, anno)
  }
}

object Cast extends Desugarable {
  override def trans(): DesugarTrans = new DesugarTrans {
    val castStatements: ListBuffer[Statement] = ListBuffer()

    override def desugarExp(exp: Exp)(implicit gensym: Gensym): Exp = exp match {
      case Cast(src, targetTyp) =>
        val desugaredSrc = desugarExp(src)
        val sym = desugaredSrc match {
          case Var(v) =>
            v
          case _ =>
            val v = Name(gensym.fresh("cast"))
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
