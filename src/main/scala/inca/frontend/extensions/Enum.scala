package inca.frontend.extensions

import inca.frontend.Frontend
import inca.frontend.core.Core._
import inca.frontend.desugar.{DesugarTrans, Desugarable}
import inca.util.Gensym

import scala.collection.mutable.ListBuffer

/** Enumerates the values of the given type */
case class Enum(ty: TypeAnno) extends Exp {
  override def freeVars: Map[Name, Option[TypeAnno]] = Map()
  override def prettyprint(implicit indent: String): String = s"enum(${ty.prettyprint})"
}

/**
 * Extension adding enum expressions to @see Parser.
 */
trait EnumFrontend extends Frontend {
  import fastparse.ScalaWhitespace._
  import fastparse._

  override protected def desugarables: Seq[Desugarable] = Enum +: super.desugarables

  override protected[frontend] def atomicExp[_: P]: P[Exp] =
    P("enum" ~ "(" ~ typeAnno ~ ")").mapWithLoc(Enum.apply) |
      super.atomicExp

  override protected[frontend] def keywords: Set[String] = super.keywords + "enum"

  override def typecheckInternal(exp: Exp, anno: Option[TypeAnno]): TypeAnno = exp match {
    case Enum(ty) =>
      TEnumeration(ty)
    case _ => super.typecheckInternal(exp, anno)
  }
}

object Enum extends Desugarable {

  override def trans(): DesugarTrans = new DesugarTrans {
    val enumStatements: ListBuffer[Statement] = ListBuffer()

    override def desugarExp(exp: Exp)(implicit gensym: Gensym): Exp = exp match {
      case Enum(ty) =>
        val sym = Name(gensym.fresh(s"enum_${ty.javastring}"))
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
